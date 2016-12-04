{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitB.FunctionTable.Spec.LaTeX where

import Control.Lens
import Control.Monad.Writer
import Control.Precondition

import Data.List as L
import Data.Text hiding (toUpper)

import Text.LaTeX as T hiding (tex,(&))
import Text.LaTeX.Base.Class hiding (fromLaTeX)
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.Hyperref

import UnitB.FunctionTable.Spec.Doc hiding (item,paragraph)
import UnitB.FunctionTable.Spec.Types as T

specToTeX :: TeXSpec -> LaTeX
specToTeX s = 
    documentclass [] article
 <> usepackage [] "multirow"
 <> usepackage [] "amsmath"
 <> usepackage [] "amssymb"
 <> usepackage [] "hyperref"
 <> usepackage [] graphicx
 -- <> title "A short message"
 -- <> author "John Short"
 -- <> author "John Short"
 -- <> comm1 "newcommand" (raw "\\dom") <> braces (textsf "dom")
 <> mathComm' "dom"
 <> renewcommand "between" 3 (raw "#1 \\le #2 \\le #3")
 -- <> renewcommand "between" 3 (raw "#1 \\1\\le #2 \\1\\le #3")
 <> s^.newCommands
 <> document (mconcat $ L.intersperse "\n" 
        $ either renderDoc (rendertex.snd) <$> contents (s^.specs))



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

instance DocFormat LaTeX where
    renderDoc (Ct t) = contentToTeX t
        where
            contentToTeX (Line ln) = fromString ln
            contentToTeX (Verbatim ln) = T.verbatim $ pack ln
            contentToTeX (Item ls) = itemize $ mconcat $ (item Nothing <>) . contentToTeX <$> ls
            contentToTeX (Enum ls) = enumerate $ mconcat $ (item Nothing <>) . contentToTeX <$> ls
            -- contentToTeX (Image _tag path) = includegraphics [] path
            contentToTeX (Image tag path) = hyperimage (createURL path) (contentToTeX tag)
            contentToTeX (Link tag path) = href [] (createURL path) (contentToTeX tag)
            contentToTeX Nil = mempty
            contentToTeX (Seq x y) = contentToTeX x <> contentToTeX y
    renderDoc (Title lvl t) 
        | lvl == 0  = title $ fromString t
        | lvl == 1  = section $ fromString t
        | lvl == 2  = subsection $ fromString t
        | lvl == 3  = subsubsection $ fromString t
        | lvl == 4  = paragraph $ fromString t
        | lvl == 5  = subparagraph $ fromString t
        | otherwise = assertFalse' "too much nesting"
