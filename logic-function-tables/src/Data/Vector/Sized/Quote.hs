{-# LANGUAGE QuasiQuotes,TemplateHaskell #-}
module Data.Vector.Sized.Quote where

import Control.Lens

import Data.List as L
import Data.Vector.Sized

import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Lens
import Language.Haskell.TH.Quote

parseVecExp :: String -> ExpQ
parseVecExp str = do
        e <- either fail return . parseExp $ "[" ++ str ++ "]"
        case e^?_ListE of
            Just xs -> L.foldr (\x xs -> [| $(pure x) :- $xs |]) [|Nil|] xs
            Nothing -> fail $ "expecting list syntax: [ _ , _ , _ ]"

parseVecPat :: String -> PatQ
parseVecPat str = do
        e <- either fail return . parsePat $ "[" ++ str ++ "]"
        case e^?_ListP of
            Just xs -> L.foldr (\x xs -> [p| $(pure x) :- $xs |]) [p|Nil|] xs
            Nothing -> fail $ "expecting list syntax: [ _ , _ , _ ]"

vec :: QuasiQuoter
vec = QuasiQuoter
    { quoteExp = parseVecExp 
    , quotePat = parseVecPat 
    , quoteType = undefined 
    , quoteDec = undefined }
