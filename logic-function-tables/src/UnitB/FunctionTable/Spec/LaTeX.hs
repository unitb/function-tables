{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitB.FunctionTable.Spec.LaTeX 
    ( module UnitB.FunctionTable.Spec.LaTeX 
    , LaTeX(TeXRaw) )
where

import Control.Lens
import Control.Monad.Writer
import Control.Precondition

import Data.List as L
import Data.Text hiding (toUpper)

import           Text.LaTeX as T hiding (tex,(&))
import qualified Text.LaTeX as T 
import Text.LaTeX.Base.Class hiding (fromLaTeX)
import Text.LaTeX.Base.Syntax

import qualified Text.LaTeX.Packages.Color as T
import           Text.LaTeX.Packages.Graphicx
import           Text.LaTeX.Packages.Hyperref

import UnitB.FunctionTable.Spec.Doc hiding (item,paragraph)
import UnitB.FunctionTable.Spec.Types as T

specToTeX :: TeXSpec () -> LaTeX
specToTeX s = 
    documentclass [] article
 <> usepackage [] "multirow"
 <> usepackage [] "amsmath"
 <> usepackage [] "array"
 <> usepackage [] "amssymb"
 <> usepackage [] "hyperref"
 <> usepackage ["normalem"] "ulem"
 <> usepackage [] graphicx
 <> usepackage ["table"] "xcolor"
 -- <> comm1 "newcommand" (raw "\\dom") <> braces (textsf "dom")
 <> mathComm' "dom"
 <> renewcommand "between" 3 (raw "#1 \\le #2 \\le #3")
 -- <> renewcommand "between" 3 (raw "#1 \\1\\le #2 \\1\\le #3")
 <> s^.newCommands
 <> document (mconcat $ L.intersperse "\n" 
        $ either (foldMap renderDoc . ($ ())) (rendertex.snd) <$> s^.specs.contents)



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

sout :: LaTeXC l => l -> l
sout = liftL $ \l -> TeXComm "sout" [FixArg l]

cellcolor :: (Render c) => c -> LaTeX
cellcolor c = TeXComm "cellcolor" [FixArg $ rendertex c]

instance DocFormat LaTeX where
    renderDoc (Ct t) = contentToTeX t
        where
            contentToTeX (Line ln) = fromString ln
            contentToTeX (Verbatim _ ln) = T.verbatim $ pack ln
            contentToTeX (Bold ln) = textbf $ foldMap contentToTeX ln
            contentToTeX (Italics ln) = emph $ foldMap contentToTeX ln
            contentToTeX (StrikeThrough ln) = sout $ foldMap contentToTeX ln
            contentToTeX (Item ls) = itemize $ mconcat $ (item Nothing <>) . contentToTeX <$> ls
            contentToTeX (Enum ls) = enumerate $ mconcat $ (item Nothing <>) . contentToTeX <$> ls
            -- contentToTeX (Image _tag path) = includegraphics [] path
            contentToTeX (Image tag path) = hyperimage (createURL path) (contentToTeX tag)
            contentToTeX (Link tag path) = href [] (createURL path) (contentToTeX tag)
            contentToTeX (DocTable t) = tabular Nothing 
                    (L.intersperse VerticalLine $ L.replicate (columns t) LeftColumn) 
                    $ mkRow (heading t) <> lnbk <> hline <> mconcat (L.intersperse lnbk $ L.map mkRow (rows t))
                where
                    toLaTeXColor Red    = T.Red
                    toLaTeXColor Yellow = T.Yellow
                    toLaTeXColor Green  = T.Green
                    mkCell (FormatCell c xs) =    maybe mempty (cellcolor . toLaTeXColor) c 
                                               <> fromString xs
                    mkRow = mkRow' . L.map mkCell
                    mkRow' [] = mempty
                    mkRow' (x:xs) = L.foldl (T.&) x xs
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
