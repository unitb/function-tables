{-# LANGUAGE QuasiQuotes,TemplateHaskell,ImplicitParams #-}
module UnitB.FunctionTable where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Data.Bitraversable
import Data.List as L
import Data.List.NonEmpty as N
import Data.Map  as M

import Logic.Expr hiding (render)
import Logic.Expr.Parser
import Logic.Proof.POGenerator as PO
import Logic.Theory
import Logic.WellDefinedness

-- import Text.LaTeX (render)
import Text.LaTeX.FunctionTable
import Text.Printf.TH
import Utilities.Syntactic
import Z3.Z3

parseTable :: ParserSetting
           -> FunctionTable LaTeXLI
           -> Either [Error] (FunctionTable Expr)
parseTable parser (Table h t) = do
        let parser0 = parser & expected_type .~ Just bool
            parser1 = parser & expected_type .~ Nothing
        h' <- fmap getExpr . parse_expr parser1 . toStringLi $ h
        let parser2 = parser & expected_type .~ Just (type_of h')
        Table h' <$> bitraverse 
                (fmap getExpr . parse_expr parser0 . toStringLi)
                (fmap getExpr . parse_expr parser2 . toStringLi)
            t

instance Syntactic LaTeXLI where
    line_info (LaTeXLI li _) = locToLI li
    traverseLineInfo f (LaTeXLI li x) = LaTeXLI <$> liLens f li <*> pure x
    after (LaTeXLI li t) = locToLI li &~ do
                                line += L.length lns
                                unless (L.length lns == 1) $ 
                                    column .= 1
                                column += L.length (L.last lns)
        where
            lns = L.lines t

toStringLi :: LaTeXLI -> StringLi
toStringLi (LaTeXLI li t) = asStringLi (locToLI li) t

verifyTable :: ParserSetting
            -> [Theory]
            -> Map Name Sort
            -> Map Name Def
            -> FunctionTable Expr
            -> IO (Map Label Validity)
verifyTable p' thys ss defs = 
        traverseWithKey discharge 
            . eval_generator 
            . with (do 
                _context $ contextOf p'
                PO.definitions defs
                _context $ empty_ctx & sorts .~ ss
                forM_ thys $ \thy -> do
                    let syn = mconcat $ L.map (view syntacticThm) $ all_theories thy
                    _context $ theory_ctx thy
                    set_syntactic_props syn
                    nameless_hyps $ M.elems $ theory_facts thy ) 
            . functionTablePO

checkTable :: ParserSetting
           -> [Theory]
           -> Map Name Sort
           -> Map Name Def
           -> FunctionTable LaTeXLI
           -> IO (Either [Error] (Map Label Validity))
checkTable p thys ss defs t = runEitherT $ do
    let p' = p &~ do
                -- decls %= 
                sorts %= M.union ss
                -- s <- use sorts
                -- traceM $ pretty s
    t' <- hoistEither $ parseTable 
            (p' & decls %~ M.union (M.mapMaybe defAsVar defs)) 
            t
    lift $ verifyTable p' thys ss defs t'

functionTablePO :: FunctionTable Expr -> POGen ()
functionTablePO (Table _ t) = functionTablePO' t

functionTablePO' :: TableCells Expr 
                 -> POGen ()
functionTablePO' t = do
    completeness t 
    disjointness t 
    wellDefinedness t
    let subtablePO i (asm,t') = with (prefix (show i) >> nameless_hyps [asm]) $ functionTablePO' t'
    imapMOf_ isubtables subtablePO t

disjointness :: TableCells Expr -> POGen ()
disjointness (Cell _) = return ()
disjointness (Condition _ xs) = sequence_ $ do
        (i,(y,_),ys) <- L.zip3 [0..] (N.toList xs) $ N.tail $ N.tails (N.zip (0:|[1..]) xs)
        (j,(z,_))      <- ys
        [emit_goal [label $ [s|disjointness-%d-%d|] i j] $ znot y `zor` znot z]

completeness :: TableCells Expr -> POGen ()
completeness (Cell _) = return ()
completeness (Condition _ xs) = emit_goal [label "completeness"] $ zsome $ fst <$> xs

wellDefinedness :: TableCells Expr -> POGen ()
wellDefinedness (Cell p) = emit_goal [label "WD"] $ well_definedness p
wellDefinedness (Condition _ xs) = imapM_ (\i -> emit_goal [label $ [s|WD/%d|] i].well_definedness.fst) xs
