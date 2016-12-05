{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Example where

import UnitB.FunctionTable

md_ft :: FunctionTable LaTeXLI 
md_ft = makeTable                                           [tex|\cMd|] $ do
            cell [tex|\INIT \lor \mSw = \sOff|]             [tex|\off|] 
                    -- there should be an error sOff is not the same time cMd
            -- cell [tex|\neg \INIT \land \neg \mSw = \sOff|] [tex|\off|]
            branch (conjList 
                    [ [tex|\neg \INIT|]
                    , [tex|\neg \mSw = \sOff|] ]) $ do
                cell [tex|\preCMd = \off|]                  [tex|\init|]
                branch [tex|\preCMd = \init|] $ do
                    cell [tex|\neg \initOk|]                [tex|\init|]
                    cell [tex|\initOk|]                     [tex|\normal|]
                branch [tex|\preCMd \in \{\normal,\fail\} |] $ do
                    cell [tex|\mSt = \valid|]               [tex|\normal|]
                    cell [tex|\mSt = \invalid|]             [tex|\fail|]

hc_ft :: FunctionTable LaTeXLI
hc_ft = makeTable                                              [tex|\cHc|] $ do
            cellH 2 [tex|\INIT \lor \mSt = \invalid \lor \neg \validRange \lor \mSw = \sOff |] 
                                                               [tex|\sOff|]
            branch (conjList 
                    [ [tex|\neg \INIT|] 
                    , [tex|\mSt = \valid|] 
                    , [tex|\validRange |]
                    , [tex| \mSw = \sOn |] ]) $ do
                cellH 2 [tex|\mTm < \mDl|]                      [tex|\sOn|]
                cellH 2 [tex|\between{\mDl}{\mTm}{\mDh}|]       [tex|\preCHc|]
                cellH 2 [tex|\mDh < \mTm|]                      [tex|\sOff|]

al_ft :: FunctionTable LaTeXLI
al_ft = makeTable                                               [tex|\cAl|] $ do
            cell [tex|\cMd \in \{ \off, \init \} |]             [tex|\sOff|]
            branch [tex|\cMd \in \{ \normal, \fail \} |] $ do
                cell [tex|\problem|]                            [tex|\sOn|] 
                branch [tex|\neg \problem|] $ do 
                    cell [tex|\neg \heldfor \lor \hysteresis|]  [tex|\preCAl|] 
                    cell [tex|\heldfor \land \neg \hysteresis|] [tex|\sOff|] 


isolette :: SpecBuilder ()
isolette = do
            enumSort "Status" [("sOff","off"),("sOn","on")]
            enumSort' "Mode"   ["off","normal","init","fail"]
            enumSort' "Validity" ["valid","invalid"]
            constant "INIT" "\\Bool"
            constant "initOk" "\\Bool"
            constant "problem"    "\\Bool"
            constant "hysteresis" "\\Bool"
            constant "heldfor" "\\Bool"
            definition "validRange" [tex| \between{\mAl}{\mDl}{\mDh} \land \mDh < \mAh |]
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