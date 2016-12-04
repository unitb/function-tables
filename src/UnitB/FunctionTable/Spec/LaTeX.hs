{-# LANGUAGE OverloadedStrings #-}
module UnitB.FunctionTable.Spec.LaTeX where

import Control.Lens
import Control.Monad.Writer

import Data.List as L
import Data.Text hiding (toUpper)

import Text.LaTeX as T hiding (tex,(&))
import Text.LaTeX.Base.Class hiding (fromLaTeX)

import UnitB.FunctionTable.Spec.Types as T

specToTeX :: TeXSpec -> LaTeX
specToTeX s = 
    documentclass [] article
 <> usepackage [] "multirow"
 <> usepackage [] "amsmath"
 <> usepackage [] "amssymb"
 -- <> title "A short message"
 -- <> author "John Short"
 -- <> author "John Short"
 -- <> comm1 "newcommand" (raw "\\dom") <> braces (textsf "dom")
 <> mathComm' "dom"
 <> renewcommand "between" 3 (raw "#1 \\le #2 \\le #3")
 -- <> renewcommand "between" 3 (raw "#1 \\1\\le #2 \\1\\le #3")
 <> s^.newCommands
 <> document (mconcat $ L.intersperse "\n" 
        $ either fromString (rendertex.snd) <$> contents (s^.specs))



mathComm' :: LaTeXC t 
          => Text 
          -> t
mathComm' txt = mathComm txt (textsf $ rendertex txt)

mathComm :: LaTeXC t 
         => Text 
         -> t 
         -> t
mathComm c txt = comm1 "newcommand" (raw $ "\\" <> c) <> braces txt

newcommand :: LaTeXC t 
           => Text 
           -> Int
           -> t 
           -> t
newcommand c n txt = commS "newcommand" 
                <> braces (raw $ "\\" <> c) 
                <> liftL (\t -> raw "[" <> t <> raw "]") (fromString $ show n)
                <> braces txt

renewcommand :: LaTeXC t 
             => Text 
             -> Int
             -> t 
             -> t
renewcommand c n txt = commS "renewcommand" 
                <> braces (raw $ "\\" <> c) 
                <> liftL (\t -> raw "[" <> t <> raw "]") (fromString $ show n)
                <> braces txt

arg :: LaTeXC t => Int -> t 
arg n = raw $ "#" <> pack (show n)
