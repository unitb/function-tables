{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Example where

import UnitB.FunctionTable as T
import UnitB.FunctionTable.Spec as T
import Text.LaTeX.FunctionTable as T

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
            cell [T.tex|\INIT \lor \mSt = \invalid \lor \neg \validRange \lor \mSw = \sOff |] 
                                                               [T.tex|\sOff|]
            branch (conjList 
                    [ [T.tex|\neg \INIT|] 
                    , [T.tex|\mSt = \valid|] 
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


isolette = do
            enumSort "Status" [("sOff","off"),("sOn","on")]
            enumSort' "Mode"   ["off","normal","init","fail"]
            enumSort' "Validity" ["valid","invalid"]
            constant "INIT" "\\Bool"
            constant "initOk" "\\Bool"
            constant "problem"    "\\Bool"
            constant "hysteresis" "\\Bool"
            constant "heldfor" "\\Bool"
            definition "validRange" [T.tex| \between{\mAl}{\mDl}{\mDh} \land \mDh < \mAh |]
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
            includeTableAsm md_ft
            "This is text" 
            "what do you mean?"
            paragraph $ do
                "This is more text"
                "What???"
                "MORE TEXT!!!"
            mapM_ includeTable [hc_ft,al_ft]