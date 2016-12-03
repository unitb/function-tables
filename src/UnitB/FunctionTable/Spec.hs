{-# LANGUAGE OverloadedStrings
        ,TemplateHaskell
        ,TypeFamilies
        ,QuasiQuotes
        ,TupleSections #-}
module UnitB.FunctionTable.Spec where

import Control.Applicative
import Control.Concurrent.Async
import Control.Lens
import Control.Lens.Misc
import Control.Monad.State
import Control.Monad.Writer
import Control.Precondition

import Data.Char
import Data.Either
import Data.List as L
import Data.Map as M
import Data.Text hiding (toUpper)

-- import GHC.Generics
import GHC.Generics.Instances

import Logic.Expr as N hiding (array)
import Logic.Expr.Parser
import Logic.QuasiQuote
import Logic.Theories

import           Pipes
import qualified Pipes.Prelude as P

import System.Process

import Text.LaTeX as T hiding (tex,(&))
import Text.LaTeX.Base.Class hiding (fromLaTeX)
import Text.LaTeX.FunctionTable as T
import Text.LaTeX.Internal.FunctionTable as T hiding (Pre)
import Text.LaTeX.Packages.AMSMath hiding (to)

import Text.Printf.TH

import UnitB.FunctionTable as T

import Utilities.Syntactic

import Z3.Z3 (Validity (..))

type Primed = Bool
type SpecE = Spec (Map Expr) Expr VarDeclE Def
type TeXSpec = Spec Content LaTeXLI VarDeclT LaTeXLI
newtype Content a = Content { contents :: [Either LaTeX a] }
    deriving (Functor,Foldable,Traversable,Generic)
data VarDeclT = VarDeclT (Type -> Type) LaTeXLI Primed
data VarDeclE = VarDeclE Var (Maybe Expr)

data Spec t a var def = Spec 
        { _newCommands :: LaTeX  
        , specPrefix  :: LaTeX 
        , _sortDecl   :: Map Name Sort
        , _dataCons   :: Map Name Def
        , _userDef    :: Map Name def
        , _userConst  :: Map Name var
        , _specs :: t (Bool,FunctionTable a) }
    deriving (Generic)

makeLenses ''Spec

class HasDecl decl v where
    decl :: Lens' decl v

instance HasDecl VarDeclT LaTeXLI where
    decl f (VarDeclT g d p) = (\d' -> VarDeclT g d' p) <$> f d
instance HasDecl VarDeclE Var where
    decl f (VarDeclE d wd) = (\d' -> VarDeclE d' wd) <$> f d

wd :: VarDeclE -> Maybe Expr
wd (VarDeclE _ d) = d

instance Monoid (Content a) where
    mappend = genericMAppend
    mempty  = genericMEmpty
instance t ~ Content => Monoid (Spec t a var def) where
    mappend = genericMAppend
    mempty  = genericMEmpty

-- traverse3 :: Applicative f
--           => (a -> f a')
--           -> (b -> f b')
--           -> (c -> f c')
--           -> Spec a b c
--           -> f (Spec a' b' c')
-- traverse3 f g h (Spec l0 l1 ss ds cs ts) = 
--         Spec l0 l1 ss <$> traverse h ds
--                       <*> traverse g cs
--                       <*> traverse (bitraverse f f) ts

-- instance Bifunctor (Spec a) where
--     bimap = bimapDefault
-- instance Bifoldable (Spec a) where
--     bifoldMap = bifoldMapDefault
-- instance Bitraversable (Spec a) where
--     bitraverse = traverse3 pure
-- instance Functor (Spec a var) where
--     fmap f = bimap id f

newtype SpecBuilder a = SpecBuilder (Writer TeXSpec a)
    deriving (Functor,Applicative,Monad)

zinit :: ExprP
zinit = Right $ Word $ Var [N.tex|\INIT|] bool

parseSpec :: TeXSpec -> Either [Error] SpecE
parseSpec s0 = do
        let ts = [ arithmetic
                 , function_theory
                 , set_theory
                 , interval_theory
                 , basic_theory]
            ctx = ctxWith ts (sorts %= M.union (s0^.sortDecl)) id
            parseVar n (VarDeclT c t primed) = do
                t <- parse_type
                        (contextOf ctx) 
                        (toStringLi t)
                        (line_info t)
                let v = Var n . c $ t
                    e | primed    = Just . fromRight' $ zIsDef (Right $ Word v) .=. mznot zinit
                      | otherwise = Nothing
                return $ VarDeclE v e
        s1 <- userConst (itraverseValidation parseVar) s0
        let mkDef n e = Def [] n [] (type_of e) e
            parseDef n e = mkDef n . getExpr <$> parse_expr parser e
            parser = ctx &~ do
                    decls %= M.union (M.map (view decl) $ s1^.userConst)
        s2 <- userDef (itraverseValidation $ lmap toStringLi . parseDef) s1
        let parser' = parser &~ do
                    decls %= M.union (M.mapMaybe defAsVar $ s2^.userDef)
                    decls %= M.union (M.mapMaybe defAsVar $ s2^.dataCons)
            makeTable = M.fromList . L.map (liftA2 (,) (header.snd) id) 
        specs (fmap makeTable . (traverseValidation._2) (parseTable parser') . rights . contents) s2
        -- specs (fmap makeTable . traverseValidation (parseTable parser')) s2

renderSpec :: SpecBuilder a 
           -> Text
renderSpec (SpecBuilder cmd) = T.render . specToDoc . execWriter $ cmd

verifySpec :: SpecBuilder a -> IO ()
verifySpec spec = runEffect $ verifySpec' Render spec >-> P.stdoutLn

data SpecOpt = Render | DoNotRender

verifySpec' :: SpecOpt
            -> SpecBuilder a 
            -> Producer String IO ()
verifySpec' opt (SpecBuilder cmd) = do
        let ss  = execWriter cmd
            thys = [ arithmetic
                   , function_theory
                   , set_theory
                   , interval_theory
                   , basic_theory]
        case opt of
            Render -> do
                liftIO $ renderFile "table.tex" (specToDoc ss)
                _ <- liftIO $ rawSystem "pdflatex" ["table.tex"]
                return ()
            DoNotRender -> return ()
        case parseSpec ss of
            Right ss' -> do
                    let ts  = ss'^.specs
                        ts' = foldMap (pure.assertion.snd) $ M.filter fst ts
                    rs <- liftIO $ flip mapConcurrently ts $ \(isAsm,t) -> do
                        let asm | isAsm     = []
                                | otherwise = ts'
                        (,) t <$> verifyTable' parser thys (ss'^.sortDecl) 
                                ((ss'^.userDef) `M.union` (ss'^.dataCons))
                                ( (asm ++) . M.elems $ M.mapMaybe wd $ ss'^.userConst )
                                t
                    total <- forM rs $ \(t,r) -> do  
                        yield $ pretty $ header t
                        mapM_ (yield . pretty) $ toList r
                        yield $ [s|Success: %d / %d |] 
                            (size $ M.filter (Valid ==) r) 
                            (size r)
                        return
                            (size $ M.filter (Valid ==) r,size r)
                    yield $ [s|Total: %d / %d |] (sum $ M.map fst total) (sum $ M.map snd total)
                where
                    parser = ctxWith thys (do
                                sorts %= M.union (ss'^.sortDecl) 
                                decls %= insert_symbol (Var [N.tex|\INIT|] bool)
                                decls %= M.union (M.map (view decl) $ ss'^.userConst)) id
            Left es -> yield . show_err $ es
        return ()

specToDoc :: TeXSpec -> LaTeX
specToDoc s = 
    documentclass [] article
 <> usepackage [] "multirow"
 <> usepackage [] "amsmath"
 <> usepackage [] "amssymb"
 -- <> title "A short message"
 -- <> author "John Short"
 -- <> author "John Short"
 -- <> comm1 "newcommand" (raw "\\dom") <> braces (textsf "dom")
 <> mathComm' "dom"
 <> renewcommand "between" 3 (raw "#1 \\le #2 \\le #3")
 -- <> renewcommand "between" 3 (raw "#1 \\1\\le #2 \\1\\le #3")
 <> s^.newCommands
 <> document (mconcat $ L.intersperse (lnbk <> lnbk :: LaTeX) 
        $ either id (rendertex.snd) <$> contents (s^.specs))


declSort :: Pre => LaTeX -> SpecBuilder ()
declSort t = SpecBuilder $ tell $ mempty 
        { _sortDecl = M.singleton n (Sort n (asInternal n) 0) }
    where n = makeName . unpack . T.render $ t

enumSort' :: Pre => LaTeX -> [String] -> SpecBuilder ()
enumSort' t = enumSort t . L.map (\l -> (l,fromString l))

enumSort :: Pre 
         => LaTeX -> [(String,LaTeX)] -> SpecBuilder ()
enumSort t cs = SpecBuilder $ tell $ mempty
        { _newCommands = mconcat [ mathComm (fromString c) (textit txt) | (c,txt) <- cs ]
        , _dataCons  = symbol_table 
            [ Def [] (makeName ("\\" ++ n)) 
                  [] t'
                  (Word $ Var (makeName n) t') 
                        | (n,_) <- cs ]
        , _sortDecl  = M.singleton n s }
    where 
        n  = makeName . unpack . T.render $ t
        s  = Datatype [] n [ (makeName $ fst c,[]) | c <- cs ]
        t' = make_type s []

constant :: Pre => LaTeX -> String -> SpecBuilder ()
constant n t = SpecBuilder $ tell $ mempty
        { _newCommands = mathComm' $ T.render n
        , _userConst = M.singleton n' (VarDeclT id (makeLaTeXLI t) False) }
    where n' = makeName $ "\\" <> unpack (T.render n)
definition :: Pre => LaTeX -> LaTeX -> SpecBuilder ()
definition n e = SpecBuilder $ tell $ mempty
        { _newCommands = mathComm (T.render n) n
        , _userDef = M.singleton n' (fromLaTeX e) }
    where n' = makeName . unpack $ "\\" <> T.render n
monitored :: Pre => LaTeX -> String -> SpecBuilder ()
monitored n t = SpecBuilder $ tell $ mempty
        { _newCommands = mconcat 
            [ mathComm ("m" <> tN') 
                       ( textsf $ "m_" <> n )
            , mathComm ("preM" <> tN') 
                       ( textsf ("m_" <> n) !: "-1" )
            ]
        , _userConst = M.fromList 
                    [(n',VarDeclT id t' False)
                    ,(preN',VarDeclT guarded_type t' True)] }
    where 
        tN' = T.render n & _Cons._1 %~ toUpper
        n'  = makeName . unpack $ "\\m" <> tN'
        preN'  = makeName . unpack $ "\\preM" <> tN'
        t'  = makeLaTeXLI t
controlled :: Pre => LaTeX -> String -> SpecBuilder ()
controlled n t = SpecBuilder $ tell $ mempty
        { _newCommands = mconcat 
            [ mathComm ("c" <> tN') 
                       ( textsf $ "c_" <> n )
            , mathComm ("preC" <> tN') 
                       ( textsf ("c_" <> n) !: "-1" )
            ]
        , _userConst = M.fromList 
                    [(n',VarDeclT id t' False)
                    ,(preN',VarDeclT guarded_type t' True)] }
    where 
        tN' = T.render n & _Cons._1 %~ toUpper
        n'  = makeName . unpack $ "\\c" <> tN'
        preN'  = makeName . unpack $ "\\preC" <> tN'
        t'  = makeLaTeXLI t

includeTable :: FunctionTable LaTeXLI -> SpecBuilder ()
includeTable t = SpecBuilder $ tell $ mempty
        { _specs = Content [Right (False,t)] } 

includeTableAsm :: FunctionTable LaTeXLI -> SpecBuilder ()
includeTableAsm t = SpecBuilder $ tell $ mempty
        { _specs = Content [Right (True,t)] } 

table :: LaTeX -> M LaTeXLI () -> SpecBuilder ()
table v t = includeTable $ makeTable v t

mathComm' :: LaTeXC t 
          => Text 
          -> t
mathComm' txt = mathComm txt (textsf $ rendertex txt)

mathComm :: LaTeXC t 
         => Text 
         -> t 
         -> t
mathComm c txt = comm1 "newcommand" (raw $ "\\" <> c) <> braces txt

newcommand :: LaTeXC t 
           => Text 
           -> Int
           -> t 
           -> t
newcommand c n txt = commS "newcommand" 
                <> braces (raw $ "\\" <> c) 
                <> liftL (\t -> raw "[" <> t <> raw "]") (fromString $ show n)
                <> braces txt

renewcommand :: LaTeXC t 
             => Text 
             -> Int
             -> t 
             -> t
renewcommand c n txt = commS "renewcommand" 
                <> braces (raw $ "\\" <> c) 
                <> liftL (\t -> raw "[" <> t <> raw "]") (fromString $ show n)
                <> braces txt

arg :: LaTeXC t => Int -> t 
arg n = raw $ "#" <> pack (show n)

text :: [LaTeX] -> SpecBuilder ()
text xs = SpecBuilder $ tell $ mempty 
        { _specs = Content [Left $ mconcat $ L.intersperse " " xs] }
