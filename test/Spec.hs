
{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Lens
import Data.Bitraversable
import Data.Map

import Logic.QuasiQuote
import Logic.Theories.Arithmetic
import Logic.Theories.SetTheory
import Logic.Theories.FunctionTheory
import Logic.Theory

import System.Exit
import System.Process

import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.FunctionTable

import UnitB.FunctionTable

import Z3.Z3

doc :: Render a => a -> LaTeX
doc t = 
    documentclass [] article
 <> usepackage [] "multirow"
 -- <> title "A short message"
 -- <> author "John Short"
 -- <> author "John Short"
 <> comm1 "newcommand" (raw "\\dom") <> braces (textsf "dom")
 <> document (rendertex t)

ft :: FunctionTable LaTeXLI
ft = makeTable "x" $ do
        cell "y < 3"                    "x + 1"
        branch "y = 3" $ do 
            cell "x \\in \\dom.f"       "f.x"
            cell "\\neg x \\in \\dom.f" "x"
        cell "y > 3"                    "x - 1"

main :: IO ()
main = do
        setNumCapabilities 8
            --
            -- Verification
            --
        let ts = [ arithmetic
                 , function_theory
                 , set_theory
                 , basic_theory]
            parser = ctxWith ts 
                (do [var| x,y : \Int |]
                    [var| f : \Int \pfun \Int |]) id
        r <- verifyTable parser ts ft
        if has (_Right.filtered ((7 ==) . size).filtered (all (Valid ==))) r then exitSuccess
                   else exitFailure
            -- 
            -- Rendering
            -- 
        -- renderFile "table.tex" (doc ft)
        -- rawSystem "pdflatex" ["table.tex"]
        return ()
