{-# LANGUAGE OverloadedStrings,TemplateHaskell,QuasiQuotes #-}
module UnitB.FunctionTable.Spec where

import Control.Lens
import Control.Lens.Misc
import Control.Monad.State
import Control.Monad.Writer
import Control.Precondition

import Data.Char
import Data.List as L
import Data.Map as M
import Data.Text hiding (toUpper)

-- import GHC.Generics
import GHC.Generics.Instances

import Logic.Expr as N hiding (array)
import Logic.Expr.Parser
import Logic.QuasiQuote
import Logic.Theories

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

type SpecE = Spec Expr Var Def
type TeXSpec = Spec LaTeXLI LaTeXLI LaTeXLI

data Spec a var def = Spec 
        { _newCommands :: LaTeX  
        , specPrefix  :: LaTeX 
        , _sortDecl   :: Map Name Sort
        , _dataCons   :: Map Name Def
        , _userDef    :: Map Name def
        , _userConst  :: Map Name var
        , _specs :: [FunctionTable a] }
    deriving (Generic)

makeLenses ''Spec

instance Monoid (Spec a var def) where
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

parseSpec :: TeXSpec -> Either [Error] SpecE
parseSpec s0 = do
        let ts = [ arithmetic
                 , function_theory
                 , set_theory
                 , interval_theory
                 , basic_theory]
            ctx = ctxWith ts (sorts %= M.union (s0^.sortDecl)) id
            parseVar n t = Var n <$> parse_type
                            (contextOf ctx) 
                            (toStringLi t)
                            (line_info t)
        s1 <- userConst (itraverseValidation parseVar) s0
        let mkDef n e = Def [] n [] (type_of e) e
            parseDef n e = mkDef n . getExpr <$> parse_expr parser e
            parser = ctx &~ do
                    decls %= M.union (s1^.userConst)
        s2 <- userDef (itraverseValidation $ lmap toStringLi . parseDef) s1
        let parser' = parser &~ do
                    decls %= M.union (M.mapMaybe defAsVar $ s2^.userDef)
                    decls %= M.union (M.mapMaybe defAsVar $ s2^.dataCons)
        (specs.traverseValidation) (parseTable parser') s2

verifySpec :: SpecBuilder a -> IO ()
verifySpec (SpecBuilder cmd) = do
        let s  = execWriter cmd
            thys = [ arithmetic
                   , function_theory
                   , set_theory
                   , interval_theory
                   , basic_theory]
        renderFile "table.tex" (specToDoc s)
        _ <- rawSystem "pdflatex" ["table.tex"]
        case parseSpec s of
            Right s' -> do
                    forM_ (s'^.specs) $ \t -> do
                        r <- verifyTable parser thys (s'^.sortDecl) 
                                ((s'^.userDef) `M.union` (s'^.dataCons))
                                t
                        mapM_ print $ toList r
                        [sP|Success: %d / %d |] 
                            (size $ M.filter (Valid ==) r) 
                            (size r)
                where
                    parser = ctxWith thys (do
                                sorts %= M.union (s'^.sortDecl) 
                                decls %= M.union (s'^.userConst)) id
            Left es -> putStrLn . show_err $ es
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
 <> document (mconcat $ L.intersperse (lnbk <> lnbk :: LaTeX) $ rendertex <$> s^.specs)


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
            [ Def [] (makeName 
                            ("\\" ++ n)) [] t'
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
        , _userConst = M.singleton n' (makeLaTeXLI t) }
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
        , _userConst = M.fromList [(n',t'),(preN',t')] }
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
        , _userConst = M.fromList [(n',t'),(preN',t')] }
    where 
        tN' = T.render n & _Cons._1 %~ toUpper
        n'  = makeName . unpack $ "\\c" <> tN'
        preN'  = makeName . unpack $ "\\preC" <> tN'
        t'  = makeLaTeXLI t

includeTable :: FunctionTable LaTeXLI -> SpecBuilder ()
includeTable t = SpecBuilder $ tell $ mempty
        { _specs = [t] } 

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
