{-# LANGUAGE QuasiQuotes
        ,OverloadedStrings
        ,LambdaCase #-}
module Main where

import Example

import Control.Concurrent
import Data.Text.IO as T (writeFile)

import UnitB.FunctionTable.Spec

main :: IO ()
main = do
        setNumCapabilities 8
        -- verifySpec isolette
        renderSpecMD isolette >>= T.writeFile "spec.md"
        return ()
