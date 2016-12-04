{-# LANGUAGE OverloadedStrings
        ,TemplateHaskell
        ,TypeFamilies
        ,QuasiQuotes
        ,TupleSections #-}
module UnitB.FunctionTable.Spec 
    ( module UnitB.FunctionTable.Spec 
    , FunctionTable )
where

import Control.Applicative
import Control.Concurrent.Async
import Control.Lens
import Control.Lens.Misc
import Control.Monad.RWS
import Control.Precondition

import Data.Char
import Data.Either
import Data.List as L
import Data.Map as M
import Data.Text hiding (toUpper)
import Data.Text.IO as T  (writeFile)

import Logic.Expr as N hiding (array)
import Logic.Expr.Parser
import Logic.QuasiQuote
import Logic.Theories

import           Pipes
import qualified Pipes.Prelude as P

import System.Process

import Text.LaTeX as T hiding (tex,(&))
import Text.LaTeX.FunctionTable as T
import Text.LaTeX.Internal.FunctionTable as T hiding (Pre)
import Text.LaTeX.Packages.AMSMath hiding (to,text)

import Text.Printf.TH

import UnitB.FunctionTable as T
import UnitB.FunctionTable.Spec.Doc as T
import UnitB.FunctionTable.Spec.LaTeX as T
import UnitB.FunctionTable.Spec.Markdown as T
import UnitB.FunctionTable.Spec.Types as T

import Utilities.Syntactic

import Z3.Z3 (Validity (..))

newtype SpecBuilder a = SpecBuilder (RWS Int TeXSpec () a)
    deriving (Functor,Applicative,Monad)

instance DocBuilder SpecBuilder where
    emitContent = liftDoc . Ct
instance a ~ () => IsString (SpecBuilder a) where
    fromString t = text t

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

renderSpecMDFile :: FilePath
                 -> SpecBuilder a 
                 -> IO ()
renderSpecMDFile fn = renderSpecMD >=> T.writeFile fn

renderSpecMD :: SpecBuilder a 
             -> IO Text
renderSpecMD (SpecBuilder cmd) = fmap unMD . specToMD . snd $ execRWS cmd 1 ()

renderSpecTeXFile :: FilePath
                  -> SpecBuilder a 
                  -> IO ()
renderSpecTeXFile fn = T.writeFile fn . renderSpecTeX

renderSpecTeX :: SpecBuilder a 
              -> Text
renderSpecTeX (SpecBuilder cmd) = T.render . specToTeX . snd $ execRWS cmd 1 ()

verifySpec :: SpecBuilder a -> IO ()
verifySpec spec = runEffect $ verifySpec' Render spec >-> P.stdoutLn

data SpecOpt = Render | DoNotRender

verifySpec' :: SpecOpt
            -> SpecBuilder a 
            -> Producer String IO ()
verifySpec' opt (SpecBuilder cmd) = do
        let ss  = snd $ execRWS cmd 1 ()
            thys = [ arithmetic
                   , function_theory
                   , set_theory
                   , interval_theory
                   , basic_theory]
        case opt of
            Render -> do
                liftIO $ renderFile "table.tex" (specToTeX ss)
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
        { _specs = Content [Left "\n\n",Right (False,t),Left "\n\n"] } 

includeTableAsm :: FunctionTable LaTeXLI -> SpecBuilder ()
includeTableAsm t = SpecBuilder $ tell $ mempty
        { _specs = Content [Left "\n\n",Right (True,t),Left "\n\n"] } 

table :: LaTeX -> M LaTeXLI () -> SpecBuilder ()
table v t = includeTable $ makeTable v t

title :: String -> SpecBuilder ()
title = liftDoc . Title 0

liftDoc :: Doc -> SpecBuilder ()
liftDoc doc = SpecBuilder $ tell $ mempty 
        { _specs = Content [Left doc] }

section :: String -> SpecBuilder a -> SpecBuilder a
section t (SpecBuilder cmd) = do
    lvl <- SpecBuilder ask
    liftDoc $ Title lvl t
    SpecBuilder $ local succ cmd
