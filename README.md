
# logic-function-tables
Linux / OSX: 
[![Build Status](https://travis-ci.org/unitb/logic-function-tables.svg?branch=master)](https://travis-ci.org/unitb/logic-function-tables)


Verification of function table specifications

## TODO
 1. ~~Adaptive cell height based on contents~~
 2. ~~Add bold, italics and strikethrough~~
 3. Color table cells based on verification results
 4. Add a command to automatically insert the verification results in the document


## Example
The following table:



![alt text](table13.png)



is specified by the following Haskell code (in README.hs):

```haskell
enumSort "Status" [("sOff","off"),("sOn","on")]
enumSort' "Mode"   ["off","normal","init","fail"]
enumSort' "Validity" ["valid","invalid"]
constant "INIT" "\\Bool"
controlled "md" "Mode"
monitored "sw" "Status"
constant "initOk" "\\Bool"
monitored "st" "Validity"
table                                                  [T.tex|\cMd| ] $ do
    cellH 2 [T.tex|\INIT \lor \mSw = \sOff| ]          [T.tex|\off| ] 
    branch (conjList 
            [ [T.tex|\neg \INIT| ]
            , [T.tex|\neg \mSw = \sOff| ] ]) $ do
        cell [T.tex|\preCMd = \off| ]                  [T.tex|\init| ]
        branch [T.tex|\preCMd = \init| ] $ do
            cell [T.tex|\neg \initOk| ]                [T.tex|\init| ]
            cell [T.tex|\initOk| ]                     [T.tex|\normal| ]
        branch [T.tex|\preCMd \in \{\normal,\fail\} | ] $ do
            cell [T.tex|\mSt = \valid| ]               [T.tex|\normal| ]
            cell [T.tex|\mSt = \invalid \lor \preCMd = \fail| ]             
                                                       [T.tex|\fail| ] 
```

The verification results can be obtained by replacing
`renderSpecMDFile "README.md"` with `verifySpec`. The above table
produces the following results:

```
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
Total: 13 / 14  
```

We inserted a disjointness problem on purpose and Z3 found it
easily.

To regenerate README.md, simply use:

```
./README.hs 
```
