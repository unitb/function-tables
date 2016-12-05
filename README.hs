#! /usr/local/bin/stack runghc
{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}

import UnitB.FunctionTable

main :: IO ()
main = do
  renderSpecMDFile "README.md" $ do
  -- verifySpec $ do
    title "logic-function-tables"
    "Linux / OSX: " >> link (image "Build Status" "https://travis-ci.org/unitb/logic-function-tables.svg?branch=master")
                 "https://travis-ci.org/unitb/logic-function-tables"
    ""
    ""
    "Verification of function table specifications"
    section "TODO" $ do
      listNum $ do
        item $ strike "Adaptive cell height based on contents"
        item $ strike "Add bold, italics and strikethrough"
        item $ strike "Make UnitB.FunctionTable the one-stop module for all the eDSL"
        item "Add support for held-for"
        item "Add a command to automatically insert the verification results in the document"
        item "Color table cells based on verification results"
    ""
    section "Example" $ do
      "The following table:"
      ""
      enumSort "Status" [("sOff","off"),("sOn","on")]
      enumSort' "Mode"   ["off","normal","init","fail"]
      enumSort' "Validity" ["valid","invalid"]
      constant "INIT" "\\Bool"
      controlled "md" "Mode"
      monitored "sw" "Status"
      constant "initOk" "\\Bool"
      monitored "st" "Validity"
      table                                            [tex|\cMd|] $ do
        cellH 2 [tex|\INIT \lor \mSw = \sOff|]         [tex|\off|] 
            -- there should be an error sOff is not the same time cMd
        -- cell [tex|\neg \INIT \land \neg \mSw = \sOff|] [tex|\off|]
        branch (conjList 
            [ [tex|\neg \INIT|]
            , [tex|\neg \mSw = \sOff|] ]) $ do
          cell [tex|\preCMd = \off|]                   [tex|\init|]
          branch [tex|\preCMd = \init|] $ do
            cell [tex|\neg \initOk|]                   [tex|\init|]
            cell [tex|\initOk|]                        [tex|\normal|]
          branch [tex|\preCMd \in \{\normal,\fail\} |] $ do
            cell [tex|\mSt = \valid|]                  [tex|\normal|]
            cell [tex|\mSt = \invalid \lor \preCMd = \fail|]             
                                                       [tex|\fail|]
      ""
      "is specified by the following Haskell code (in README.hs):"
      ""
      [syntax|haskell|
        enumSort "Status" [("sOff","off"),("sOn","on")]
        enumSort' "Mode"   ["off","normal","init","fail"]
        enumSort' "Validity" ["valid","invalid"]
        constant "INIT" "\\Bool"
        controlled "md" "Mode"
        monitored "sw" "Status"
        constant "initOk" "\\Bool"
        monitored "st" "Validity"
        table                                            [tex|\cMd| ] $ do
          cellH 2 [tex|\INIT \lor \mSw = \sOff| ]        [tex|\off| ] 
          branch (conjList 
              [ [tex|\neg \INIT| ]
              , [tex|\neg \mSw = \sOff| ] ]) $ do
            cell [tex|\preCMd = \off| ]                  [tex|\init| ]
            branch [tex|\preCMd = \init| ] $ do
              cell [tex|\neg \initOk| ]                  [tex|\init| ]
              cell [tex|\initOk| ]                       [tex|\normal| ]
            branch [tex|\preCMd \in \{\normal,\fail\} | ] $ do
              cell [tex|\mSt = \valid| ]                 [tex|\normal| ]
              cell [tex|\mSt = \invalid \lor \preCMd = \fail| ]             
                                                         [tex|\fail| ] |]
      ""
      "The verification results can be obtained by replacing" 
      "`renderSpecMDFile \"README.md\"` with `verifySpec`. The above table"
      "produces the following results:"
      ""
      [verbatim|
        \cMd
        (1/1/completeness,Valid)
        (1/1/disjointness-0-1,Valid)
        (1/2/WD/1,Valid)
        (1/2/completeness,Valid)
        (1/2/disjointness-0-1,ValUnknown)
        (1/WD/0,Valid)
        (1/WD/1,Valid)
        (1/WD/2,Valid)
        (1/completeness,Valid)
        (1/disjointness-0-1,Valid)
        (1/disjointness-0-2,Valid)
        (1/disjointness-1-2,Valid)
        (completeness,Valid)
        (disjointness-0-1,Valid)
        Success: 13 / 14 
        Total: 13 / 14  |]
      ""
      "We inserted a disjointness problem on purpose and Z3 found it"
      "easily."
      ""
      "To regenerate README.md, simply use:"
      ""
      [verbatim| ./README.hs |]
