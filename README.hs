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
        item $ strike "improve the parsing of expressions when arrays are provided: \\array{r{}@l}"
        item "add support for quasi quoters in literate code"
        item "Add a command to automatically insert the verification results in the document"
        item "Add support for held-for"
        item "Color table cells based on verification results"
    ""
    section "Example" $ do
      "The following table:"
      ""
      code <- [exec| 
         do enumSort "Status" [("sOff","off"),("sOn","on")]
            enumSort' "Mode"   ["off","normal","init","fail"]
            enumSort' "Validity" ["valid","invalid"]
            constant "INIT" "\\Bool"
            controlled "md" "Mode"
            monitored "sw" "Status"
            constant "initOk" "\\Bool"
            monitored "st" "Validity"
            table                                               (raw "\\cMd") $ do
              cellH 2 (raw "\\INIT \\lor \\mSw = \\sOff")       (raw "\\off") 
              branch (conjList 
                  [ (raw "\\neg \\INIT")
                  , (raw "\\neg \\mSw = \\sOff") ]) $ do
                cell (raw "\\preCMd = \\off")                   (raw "\\init")
                branch (raw "\\preCMd = \\init") $ do
                  cell (raw "\\neg \\initOk")                   (raw "\\init")
                  cell (raw "\\initOk")                         (raw "\\normal")
                branch (raw "\\preCMd \\in \\{\\normal,\\fail\\} ") $ do
                  cell (raw "\\mSt = \\valid")                  (raw "\\normal")
                  cell (raw "\\mSt = \\invalid \\lor \\preCMd = \\fail")             
                                                                (raw "\\fail") |]
      ""
      "is specified by the following Haskell code (in README.hs):"
      ""
      code
      -- [syntax|haskell|
      --   enumSort "Status" [("sOff","off"),("sOn","on")]
      --   enumSort' "Mode"   ["off","normal","init","fail"]
      --   enumSort' "Validity" ["valid","invalid"]
      --   constant "INIT" "\\Bool"
      --   controlled "md" "Mode"
      --   monitored "sw" "Status"
      --   constant "initOk" "\\Bool"
      --   monitored "st" "Validity"
      --   table                                            [tex|\cMd| ] $ do
      --     cellH 2 [tex|\INIT \lor \mSw = \sOff| ]        [tex|\off| ] 
      --     branch (conjList 
      --         [ [tex|\neg \INIT| ]
      --         , [tex|\neg \mSw = \sOff| ] ]) $ do
      --       cell [tex|\preCMd = \off| ]                  [tex|\init| ]
      --       branch [tex|\preCMd = \init| ] $ do
      --         cell [tex|\neg \initOk| ]                  [tex|\init| ]
      --         cell [tex|\initOk| ]                       [tex|\normal| ]
      --       branch [tex|\preCMd \in \{\normal,\fail\} | ] $ do
      --         cell [tex|\mSt = \valid| ]                 [tex|\normal| ]
      --         cell [tex|\mSt = \invalid \lor \preCMd = \fail| ]             
      --                                                    [tex|\fail| ] |]
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
