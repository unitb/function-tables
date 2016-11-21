{-# LANGUAGE OverloadedStrings,QuasiQuotes
        ,TemplateHaskell
        ,FlexibleInstances
        ,UndecidableInstances
        ,MultiParamTypeClasses
        ,FunctionalDependencies
        ,GeneralizedNewtypeDeriving #-}
module Text.LaTeX.FunctionTable 
    ( makeTable 
    , LaTeXLI (..)
    , FunctionTable 
    , FunctionTable' (..)
    , TableCells
    , TableCells' (..)
    , branch, cell 
    , subtables
    , isubtables
    )
where

import Text.LaTeX.Internal.FunctionTable
