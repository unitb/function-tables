{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Main where

import Control.Concurrent

import Data.List as L
import Data.Text as T (Text,intercalate)

import Example

import System.Exit

import UnitB.FunctionTable.Spec

import qualified Pipes.Prelude as P

import Test.UnitTest

suite :: TestCase
suite = test_cases 
            "Verification of function tables"
            [ aCase "verification" case0 result0
            , aCase "LaTeX rendering" case1 result1 ]

case0 :: IO String
case0 = fmap L.unlines . P.toListM $ verifySpec' DoNotRender isolette

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
case1 = return $ renderSpecTeX isolette

result1 :: Text
result1 = T.intercalate "\n"
    [ "\\documentclass{article}\\usepackage{multirow}\\usepackage{amsmath}\\usepackage{array}\\usepackage{amssymb}\\usepackage{hyperref}\\usepackage[normalem]{ulem}\\usepackage{graphicx}\\newcommand{\\dom}{\\textsf{dom}}\\renewcommand{\\between}[3]{#1 \\le #2 \\le #3}\\newcommand{\\sOff}{\\textit{off}}\\newcommand{\\sOn}{\\textit{on}}\\newcommand{\\off}{\\textit{off}}\\newcommand{\\normal}{\\textit{normal}}\\newcommand{\\init}{\\textit{init}}\\newcommand{\\fail}{\\textit{fail}}\\newcommand{\\valid}{\\textit{valid}}\\newcommand{\\invalid}{\\textit{invalid}}\\newcommand{\\INIT}{\\textsf{INIT}}\\newcommand{\\initOk}{\\textsf{initOk}}\\newcommand{\\problem}{\\textsf{problem}}\\newcommand{\\hysteresis}{\\textsf{hysteresis}}\\newcommand{\\heldfor}{\\textsf{heldfor}}\\newcommand{\\validRange}{validRange}\\newcommand{\\cMd}{\\textsf{c\\_{}md}}\\newcommand{\\preCMd}{{\\textsf{c\\_{}md}}_{-1}}\\newcommand{\\cHc}{\\textsf{c\\_{}hc}}\\newcommand{\\preCHc}{{\\textsf{c\\_{}hc}}_{-1}}\\newcommand{\\cAl}{\\textsf{c\\_{}al}}\\newcommand{\\preCAl}{{\\textsf{c\\_{}al}}_{-1}}\\newcommand{\\mSw}{\\textsf{m\\_{}sw}}\\newcommand{\\preMSw}{{\\textsf{m\\_{}sw}}_{-1}}\\newcommand{\\mSt}{\\textsf{m\\_{}st}}\\newcommand{\\preMSt}{{\\textsf{m\\_{}st}}_{-1}}\\newcommand{\\mTm}{\\textsf{m\\_{}tm}}\\newcommand{\\preMTm}{{\\textsf{m\\_{}tm}}_{-1}}\\newcommand{\\mDl}{\\textsf{m\\_{}dl}}\\newcommand{\\preMDl}{{\\textsf{m\\_{}dl}}_{-1}}\\newcommand{\\mDh}{\\textsf{m\\_{}dh}}\\newcommand{\\preMDh}{{\\textsf{m\\_{}dh}}_{-1}}\\newcommand{\\mAl}{\\textsf{m\\_{}al}}\\newcommand{\\preMAl}{{\\textsf{m\\_{}al}}_{-1}}\\newcommand{\\mAh}{\\textsf{m\\_{}ah}}\\newcommand{\\preMAh}{{\\textsf{m\\_{}ah}}_{-1}}\\begin{document}"
    , ""
    , ""
    , "\\begin{tabular}{|l|l|l||c||}\\hline \\multicolumn{3}{|l||}{}&\\multicolumn{1}{|l|}{$\\cMd$}\\\\\\cline{1-4}\\hline \\hline \\multicolumn{3}{|l||}{$\\INIT \\lor \\mSw = \\sOff$}&\\multicolumn{1}{|l|}{$\\off$}\\\\\\cline{1-4}\\multicolumn{1}{|l|}{\\multirow{5}{*}{$\\begin{array}{r@{~}l}&\\neg \\INIT\\\\\\land&\\neg \\mSw = \\sOff\\end{array}$}}&\\multicolumn{2}{|l||}{$\\preCMd = \\off$}&\\multicolumn{1}{|l|}{$\\init$}\\\\\\cline{2-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\preCMd = \\init$}}&\\multicolumn{1}{|l||}{$\\neg \\initOk$}&\\multicolumn{1}{|l|}{$\\init$}\\\\\\cline{3-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{$\\initOk$}&\\multicolumn{1}{|l|}{$\\normal$}\\\\\\cline{2-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\preCMd \\in \\{\\normal,\\fail\\} $}}&\\multicolumn{1}{|l||}{$\\mSt = \\valid$}&\\multicolumn{1}{|l|}{$\\normal$}\\\\\\cline{3-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{$\\mSt = \\invalid$}&\\multicolumn{1}{|l|}{$\\fail$}\\\\\\cline{1-4}\\end{tabular}"
    , ""
    , ""
    , ""
    , "This is text"
    , "what do you mean?"
    , ""
    , ""
    , "This is more text"
    , "foo"
    , "bar"
    , "What???"
    , "MORE TEXT!!!"
    , ""
    , ""
    , "\\begin{verbatim}paragraph $ do"
    , "    let x = \"foo\""
    , "    \"This is more text\""
    , "    x"
    , "    y"
    , "    \"What???\""
    , "    \"MORE TEXT!!!\" \\end{verbatim}"
    , "bar"
    , ""
    , ""
    , ""
    , "\\begin{tabular}{|l|l||c||}\\hline \\multicolumn{2}{|l||}{}&\\multicolumn{1}{|l|}{$\\cHc$}\\\\\\cline{1-3}\\hline \\hline \\multicolumn{2}{|l||}{\\multirow{2}{*}{$\\INIT \\lor \\mSt = \\invalid \\lor \\neg \\validRange \\lor \\mSw = \\sOff $}}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\sOff$}}\\\\\\multicolumn{2}{|l||}{}&\\multicolumn{1}{|l|}{}\\\\\\cline{1-3}\\multicolumn{1}{|l|}{\\multirow{6}{*}{$\\begin{array}{r@{~}l}&\\neg \\INIT\\\\\\land&\\mSt = \\valid\\\\\\land&\\validRange \\\\\\land& \\mSw = \\sOn \\end{array}$}}&\\multicolumn{1}{|l||}{\\multirow{2}{*}{$\\mTm < \\mDl$}}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\sOn$}}\\\\\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{}&\\multicolumn{1}{|l|}{}\\\\\\cline{2-3}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{\\multirow{2}{*}{$\\between{\\mDl}{\\mTm}{\\mDh}$}}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\preCHc$}}\\\\\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{}&\\multicolumn{1}{|l|}{}\\\\\\cline{2-3}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{\\multirow{2}{*}{$\\mDh < \\mTm$}}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\sOff$}}\\\\\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{}&\\multicolumn{1}{|l|}{}\\\\\\cline{1-3}\\end{tabular}"
    , ""
    , ""
    , ""
    , ""
    , ""
    , ""
    , "\\begin{tabular}{|l|l|l||c||}\\hline \\multicolumn{3}{|l||}{}&\\multicolumn{1}{|l|}{$\\cAl$}\\\\\\cline{1-4}\\hline \\hline \\multicolumn{3}{|l||}{$\\cMd \\in \\{ \\off, \\init \\} $}&\\multicolumn{1}{|l|}{$\\sOff$}\\\\\\cline{1-4}\\multicolumn{1}{|l|}{\\multirow{3}{*}{$\\cMd \\in \\{ \\normal, \\fail \\} $}}&\\multicolumn{2}{|l||}{$\\problem$}&\\multicolumn{1}{|l|}{$\\sOn$}\\\\\\cline{2-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{\\multirow{2}{*}{$\\neg \\problem$}}&\\multicolumn{1}{|l||}{$\\neg \\heldfor \\lor \\hysteresis$}&\\multicolumn{1}{|l|}{$\\preCAl$}\\\\\\cline{3-4}\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l|}{}&\\multicolumn{1}{|l||}{$\\heldfor \\land \\neg \\hysteresis$}&\\multicolumn{1}{|l|}{$\\sOff$}\\\\\\cline{1-4}\\end{tabular}"
    , ""
    , ""
    , "\\end{document}"
    ]

main :: IO ()
main = do
        setNumCapabilities 8
        clear_results
        r <- run_test_cases suite
        _ <- if r 
            then exitSuccess
            else exitFailure
        return ()
