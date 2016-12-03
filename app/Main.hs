{-# LANGUAGE QuasiQuotes
        ,OverloadedStrings
        ,LambdaCase #-}
module Main where

import Example

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
import Text.LaTeX.Packages.AMSMath hiding (text)

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
        verifySpec isolette
        return ()
