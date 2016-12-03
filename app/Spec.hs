{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Lens

import Data.Bitraversable
import Data.List as L
import Data.Map as M
import Data.Text (Text)

import Example

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
import UnitB.FunctionTable.Spec

import           Pipes
import qualified Pipes.Prelude as P

import Test.UnitTest

import Z3.Z3

-- doc :: Render a => a -> LaTeX
-- doc t = 
--     documentclass [] article
--  <> usepackage [] "multirow"
--  -- <> title "A short message"
--  -- <> author "John Short"
--  -- <> author "John Short"
--  <> comm1 "newcommand" (raw "\\dom") <> braces (textsf "dom")
--  <> document (rendertex t)

-- ft :: FunctionTable LaTeXLI
-- ft = makeTable "x" $ do
--         cell "y < 3"                    "x + 1"
--         branch "y = 3" $ do 
--             cell "x \\in \\dom.f"       "f.x"
--             cell "\\neg x \\in \\dom.f" "x"
--         cell "y > 3"                    "x - 1"

suite :: TestCase
suite = test_cases 
            "Verification of function tables"
            [ aCase "verification" case0 result0
            , aCase "LaTeX rendering" case1 result1 ]

case0 :: IO String
case0 = fmap unlines . P.toListM $ verifySpec' DoNotRender isolette

result0 :: String
result0 = L.unlines
            [ "\\cAl"
            , "(1/1/0/WD,Valid)"
            , "(1/1/completeness,Valid)"
            , "(1/1/disjointness-0-1,Valid)"
            , "(1/completeness,Valid)"
            , "(1/disjointness-0-1,Valid)"
            , "(completeness,Valid)"
            , "(disjointness-0-1,Valid)"
            , "Success: 7 / 7 "
            , "\\cHc"
            , "(1/1/WD,Valid)"
            , "(1/completeness,Valid)"
            , "(1/disjointness-0-1,Valid)"
            , "(1/disjointness-0-2,Valid)"
            , "(1/disjointness-1-2,Valid)"
            , "(completeness,Valid)"
            , "(disjointness-0-1,Valid)"
            , "Success: 7 / 7 "
            , "\\cMd"
            , "(1/1/completeness,Valid)"
            , "(1/1/disjointness-0-1,Valid)"
            , "(1/2/completeness,Valid)"
            , "(1/2/disjointness-0-1,Valid)"
            , "(1/WD/0,Valid)"
            , "(1/WD/1,Valid)"
            , "(1/WD/2,Valid)"
            , "(1/completeness,Valid)"
            , "(1/disjointness-0-1,Valid)"
            , "(1/disjointness-0-2,Valid)"
            , "(1/disjointness-1-2,Valid)"
            , "(completeness,Valid)"
            , "(disjointness-0-1,Valid)"
            , "Success: 13 / 13 "
            , "Total: 27 / 27 "
            ]

case1 :: IO Text
case1 = return $ renderSpec isolette

result1 :: Text
result1 = mconcat
    [ "\\documentclass{article}\\usepackage{multirow}\\usepackage{amsmath}"
    , "\\usepackage{amssymb}"
    , "\\newcommand{\\dom}{\\textsf{dom}}"
    , "\\renewcommand{\\between}[3]{#1 \\le #2 \\le #3}"
    , "\\newcommand{\\sOff}{\\textit{off}}"
    , "\\newcommand{\\sOn}{\\textit{on}}"
    , "\\newcommand{\\off}{\\textit{off}}"
    , "\\newcommand{\\normal}{\\textit{normal}}"
    , "\\newcommand{\\init}{\\textit{init}}"
    , "\\newcommand{\\fail}{\\textit{fail}}"
    , "\\newcommand{\\valid}{\\textit{valid}}"
    , "\\newcommand{\\invalid}{\\textit{invalid}}"
    , "\\newcommand{\\INIT}{\\textsf{INIT}}"
    , "\\newcommand{\\initOk}{\\textsf{initOk}}"
    , "\\newcommand{\\problem}{\\textsf{problem}}"
    , "\\newcommand{\\hysteresis}{\\textsf{hysteresis}}"
    , "\\newcommand{\\heldfor}{\\textsf{heldfor}}"
    , "\\newcommand{\\validRange}{validRange}"
    , "\\newcommand{\\cMd}{\\textsf{c\\_{}md}}"
    , "\\newcommand{\\preCMd}{{\\textsf{c\\_{}md}}_{-1}}"
    , "\\newcommand{\\cHc}{\\textsf{c\\_{}hc}}"
    , "\\newcommand{\\preCHc}{{\\textsf{c\\_{}hc}}_{-1}}"
    , "\\newcommand{\\cAl}{\\textsf{c\\_{}al}}"
    , "\\newcommand{\\preCAl}{{\\textsf{c\\_{}al}}_{-1}}"
    , "\\newcommand{\\mSw}{\\textsf{m\\_{}sw}}"
    , "\\newcommand{\\preMSw}{{\\textsf{m\\_{}sw}}_{-1}}"
    , "\\newcommand{\\mSt}{\\textsf{m\\_{}st}}"
    , "\\newcommand{\\preMSt}{{\\textsf{m\\_{}st}}_{-1}}"
    , "\\newcommand{\\mTm}{\\textsf{m\\_{}tm}}"
    , "\\newcommand{\\preMTm}{{\\textsf{m\\_{}tm}}_{-1}}"
    , "\\newcommand{\\mDl}{\\textsf{m\\_{}dl}}"
    , "\\newcommand{\\preMDl}{{\\textsf{m\\_{}dl}}_{-1}}"
    , "\\newcommand{\\mDh}{\\textsf{m\\_{}dh}}"
    , "\\newcommand{\\preMDh}{{\\textsf{m\\_{}dh}}_{-1}}"
    , "\\newcommand{\\mAl}{\\textsf{m\\_{}al}}"
    , "\\newcommand{\\preMAl}{{\\textsf{m\\_{}al}}_{-1}}"
    , "\\newcommand{\\mAh}{\\textsf{m\\_{}ah}}"
    , "\\newcommand{\\preMAh}{{\\textsf{m\\_{}ah}}_{-1}}"
    , "\\begin{document}"
    , "\\begin{tabular}{|l|l|l||c||}\\hline \\multicolumn{3}{|l||}{}&$\\cMd$"
    , "\\\\\\cline{1-4}\\hline \\hline \\multicolumn{3}{|l||}{$\\INIT \\lor \\mSw = \\sOff$}&$\\off$"
    , "\\\\\\cline{1-4}\\multicolumn{1}{|l|}{\\multirow{5}{*}{$\\begin{array}{rl}&\\neg \\INIT"
    , "\\\\\\land&\\neg \\mSw = \\sOff\\end{array}$}}&\\multicolumn{2}{|l||}{$\\preCMd = \\off$}&$\\init$"
    , "\\\\\\cline{2-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\preCMd = \\init$}}&\\multicolumn{1}{|l||}{$\\neg \\initOk$}&$\\init$"
    , "\\\\\\cline{3-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{$\\initOk$}&$\\normal$"
    , "\\\\\\cline{2-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\preCMd \\in \\{\\normal,\\fail\\} $}}&\\multicolumn{1}{|l||}{$\\mSt = \\valid$}&$\\normal$"
    , "\\\\\\cline{3-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{$\\mSt = \\invalid$}&$\\fail$"
    , "\\\\\\cline{1-4}\\end{tabular}"
    , "\\\\"
    , "\\\\This is text what do you mean?"
    , "\\\\"
    , "\\\\"
    , "\\begin{tabular}{|l|l||c||}\\hline \\multicolumn{2}{|l||}{}&$\\cHc$"
    , "\\\\\\cline{1-3}\\hline \\hline \\multicolumn{2}{|l||}{$\\INIT \\lor \\mSt = \\invalid \\lor \\neg \\validRange \\lor \\mSw = \\sOff $}&$\\sOff$"
    , "\\\\\\cline{1-3}\\multicolumn{1}{|l|}{\\multirow{3}{*}{$\\begin{array}{rl}&\\neg \\INIT"
    , "\\\\\\land&\\mSt = \\valid"
    , "\\\\\\land&\\validRange "
    , "\\\\\\land& \\mSw = \\sOn \\end{array}$}}&\\multicolumn{1}{|l||}{$\\mTm < \\mDl$}&$\\sOn$"
    , "\\\\\\cline{2-3}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{$\\between{\\mDl}{\\mTm}{\\mDh}$}&$\\preCHc$"
    , "\\\\\\cline{2-3}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{$\\mDh < \\mTm$}&$\\sOff$"
    , "\\\\\\cline{1-3}\\end{tabular}"
    , "\\\\"
    , "\\\\"
    , "\\begin{tabular}{|l|l|l||c||}\\hline \\multicolumn{3}{|l||}{}&$\\cAl$"
    , "\\\\\\cline{1-4}\\hline \\hline \\multicolumn{3}{|l||}{$\\cMd \\in \\{ \\off, \\init \\} $}&$\\sOff$"
    , "\\\\\\cline{1-4}\\multicolumn{1}{|l|}{\\multirow{3}{*}{$\\cMd \\in \\{ \\normal, \\fail \\} $}}&\\multicolumn{2}{|l||}{$\\problem$}&$\\sOn$"
    , "\\\\\\cline{2-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\neg \\problem$}}&\\multicolumn{1}{|l||}{$\\neg \\heldfor \\lor \\hysteresis$}&$\\preCAl$"
    , "\\\\\\cline{3-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{$\\heldfor \\land \\neg \\hysteresis$}&$\\sOff$"
    , "\\\\\\cline{1-4}\\end{tabular}\\end{document}"
    ]




main :: IO ()
main = do
        setNumCapabilities 8
            --
            -- Verification
            --
        -- let ts = [ arithmetic
        --          , function_theory
        --          , set_theory
        --          , basic_theory]
        --     parser = ctxWith ts 
        --         (do [var| x,y : \Int |]
        --             [var| f : \Int \pfun \Int |]) id
        clear_results
        r <- run_test_cases suite
        if r 
            then exitSuccess
            else exitFailure
            -- 
            -- Rendering
            -- 
        -- renderFile "table.tex" (doc ft)
        -- rawSystem "pdflatex" ["table.tex"]
        return ()
