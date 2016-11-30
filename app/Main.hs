{-# LANGUAGE QuasiQuotes
        ,OverloadedStrings
        ,LambdaCase #-}
module Main where

import Control.Concurrent
import Control.Lens hiding ((&))
import Control.Monad.State
import Data.List as L
import Data.Map as M

import Logic.Expr as N hiding (array)
import Logic.Expr.Parser
import Logic.QuasiQuote
import Logic.Theories

import UnitB.FunctionTable as T
import UnitB.FunctionTable.Spec

import Text.LaTeX hiding (tex)
import Text.LaTeX.Base.Class
import Text.LaTeX.FunctionTable as T
import Text.LaTeX.Packages.AMSMath

import Text.Printf.TH

-- import System.Process
import Utilities.Syntactic

import Z3.Z3 (Validity(..))

doc :: Render a => a -> LaTeX
doc t = 
    documentclass [] article
 <> usepackage [] "multirow"
 <> usepackage [] "amsmath"
 <> usepackage [] "amssymb"
 -- <> title "A short message"
 -- <> author "John Short"
 -- <> author "John Short"
 <> mathComm' "dom"
 <> mathComm' "valid"
 <> mathComm' "invalid"
 <> mathComm "initOk" "init_ok"
 <> mathComm "sOff" (textsf "off")
 <> mathComm "sOn"  (textsf "on")
 <> mathComm' "normal"
 <> mathComm' "off"
 <> mathComm' "fail"
 <> mathComm' "init"
 <> mathComm' "INIT"
 <> mathComm "validRange" "valid_range"
 <> mathComm "cMd" (textsf "c_md")
 <> mathComm "preCMd" (textsf "c_md" !: "-1")
 <> mathComm "cHc" (textsf "c_hc")
 <> mathComm "preCHc" (textsf "c_hc" !: "-1")
 <> mathComm "mTm" (textsf "c_tm")
 <> mathComm "mDl" (textsf "c_dl")
 <> mathComm "mDh" (textsf "c_dh")
 <> mathComm "mAl" (textsf "c_al")
 <> mathComm "mAh" (textsf "c_ah")
 <> mathComm "mSw" (textsf "m_sw")
 <> mathComm "mSt" (textsf "m_st")
 <> document (rendertex t)

ft :: FunctionTable LaTeXLI
ft = makeTable "x" $ do
        cell "y < 3"                    "x + 1"
        branch "y = 3" $ do 
            cell "x \\in \\dom.f"       "f.x"
            cell "\\neg x \\in \\dom.f" "x"
        cell "y > 3"                    "x - 1"

conjList :: (Monoid t,LaTeXC t) => [t] -> t
conjList [] = mempty
conjList (x:xs) = array Nothing [RightColumn,LeftColumn] $ 
-- conjList (x:xs) = array Nothing [RightColumn,Separator "",LeftColumn] $ 
        mconcat $ intersperse lnbk $ (mempty & x) : L.map (raw "\\land" &) xs

md_ft :: FunctionTable LaTeXLI 
md_ft = makeTable                                            [T.tex|\cMd|] $ do
            cell [T.tex|\INIT \lor \mSw = \sOff|]            [T.tex|\off|] 
                    -- there should be an error sOff is not the same time cMd
            -- cell [T.tex|\neg \INIT \land \neg \mSw = \sOff|] [T.tex|\off|]
            branch (conjList 
                    [ [T.tex|\neg \INIT|]
                    , [T.tex|\neg \mSw = \sOff|] ]) $ do
                cell [T.tex|\preCMd = \off|]                  [T.tex|\init|]
                branch [T.tex|\preCMd = \init|] $ do
                    cell [T.tex|\neg \initOk|]                [T.tex|\init|]
                    cell [T.tex|\initOk|]                     [T.tex|\normal|]
                branch [T.tex|\preCMd \in \{\normal,\fail\} |] $ do
                    cell [T.tex|\mSt = \valid|]               [T.tex|\normal|]
                    cell [T.tex|\mSt = \invalid|]             [T.tex|\fail|]

hc_ft :: FunctionTable LaTeXLI
hc_ft = makeTable                                              [T.tex|\cHc|] $ do
            cell [T.tex|\mSt = \invalid \lor \neg \validRange \lor \mSw = \sOff |] 
                                                               [T.tex|\sOff|]
            branch (conjList 
                    [ [T.tex|\mSt = \valid|] 
                    , [T.tex|\validRange |]
                    , [T.tex| \mSw = \sOn |] ]) $ do
                cell [T.tex|\mTm < \mDl|]                      [T.tex|\sOn|]
                cell [T.tex|\between{\mDl}{\mTm}{\mDh}|]       [T.tex|\preCHc|]
                cell [T.tex|\mDh < \mTm|]                      [T.tex|\sOff|]

al_ft :: FunctionTable LaTeXLI
al_ft = makeTable                                                 [T.tex|\cAl|] $ do
            cell [T.tex|\cMd \in \{ \off, \init \} |]             [T.tex|\sOff|]
            branch [T.tex|\cMd \in \{ \normal, \fail \} |] $ do
                cell [T.tex|\problem|]                            [T.tex|\sOn|] 
                branch [T.tex|\neg \problem|] $ do 
                    cell [T.tex|\neg \heldfor \lor \hysteresis|]  [T.tex|\preCAl|] 
                    cell [T.tex|\heldfor \land \neg \hysteresis|] [T.tex|\sOff|] 

verify :: FunctionTable LaTeXLI 
       -> Map Name Sort
       -> Map Name Def
       -> State ParserSetting a
       -> IO ()
verify ft ss defs vars = do
        let ts = [ arithmetic
                 , function_theory
                 , set_theory
                 , interval_theory
                 , basic_theory]
            parser = ctxWith ts (sorts %= M.union ss >> vars) id
        checkTable parser ts ss defs
                -- (PO.definitions defs) 
                ft >>= \case
            Left es -> putStrLn $ show_err es
            Right r -> do
                mapM_ print $ toList r
                [sP|Success: %d / %d |] 
                    (size $ M.filter (Valid ==) r) 
                    (size r)

main :: IO ()
main = do
        setNumCapabilities 8
        verifySpec $ do
            enumSort "Status" [("sOff","off"),("sOn","on")]
            enumSort' "Mode"   ["off","normal","init","fail"]
            enumSort' "Validity" ["valid","invalid"]
            constant "INIT" "\\Bool"
            constant "initOk" "\\Bool"
            constant "problem"    "\\Bool"
            constant "hysteresis" "\\Bool"
            constant "heldfor" "\\Bool"
            definition "validRange" [T.tex| \between{\mAl}{\mDl}{\mDh} \land \mDh < \mAh |]
            -- constant "validRange" "\\Bool"
            controlled "md" "Mode"
            controlled "hc" "Status"
            controlled "al" "Status"
            monitored "sw" "Status"
            monitored "st" "Validity"
            monitored "tm" "\\Int"
            monitored "dl" "\\Int"
            monitored "dh" "\\Int"
            monitored "al" "\\Int"
            monitored "ah" "\\Int"
        --             [var| \mSt : Validity |]
        --             [var| \mTm,\mDl,\mDh,\mAl,\mAh : \Int |]
            mapM_ includeTable [md_ft,hc_ft,al_ft]
            --
            -- Verification
            --
        return ()
