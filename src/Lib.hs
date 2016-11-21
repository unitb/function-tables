module Lib where

import Control.Monad

import Logic.Expr
import Logic.Proof.POGenerator
import Logic.Expr.Parser
import Logic.Expr.Printable

import Text.LaTeX.FunctionTable
import Utilities.Syntactic
import Z3.Z3

parseTable :: ParserSetting
           -> FunctionTable StringLi 
           -> Either [Error] (FunctionTable Expr)
parseTable parser = traverse (fmap getExpr . parse_expr parser)

verifyTable :: TableCells Expr -> POGen ()
verifyTable t = completeness t >> disjointness t >> wellDefinedness

completeness :: TableCells Expr -> POGen ()
