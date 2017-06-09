{-# LANGUAGE OverloadedStrings,QuasiQuotes #-}
module UnitB.FunctionTable.Spec.Markdown where

import Control.Lens
import Control.Monad.Writer
import Data.Bifunctor
import Data.List as L
import Data.Text as T (pack,unpack,replicate,unlines)

import TeX2PNG

import System.Directory
import System.FilePath

import Text.LaTeX hiding ((&),between)
import Text.LaTeX.FunctionTable
import Text.Printf.TH

import UnitB.FunctionTable.Spec.Doc
import UnitB.FunctionTable.Spec.LaTeX
import UnitB.FunctionTable.Spec.Types

newtype Markdown = Markdown { unMD :: Text }
    deriving (Eq,Ord,Monoid,IsString)

specToMD :: TeXSpec () -> IO Markdown
specToMD sys = runDocT $ do
        cd <- liftIO getCurrentDirectory
        forM_ (zip [0..] $ sys^.specs.contents) $ \(i,ln) -> do
            case ln of
                Left  str   -> do
                    tell $ str ()
                    -- return Nothing
                Right (_b,tbl) -> do
                    r <- lift $ tableImage ([s|table%d|] i) ps def tbl
                    -- either _ (tell [Ct $ Image "alt text" $ [s|table%d|] i]) r
                    either (fail . unpack) 
                           (tell . pure . Ct . Image "alt text" . makeRelative cd) 
                           r
                    -- return $ Just (i,r)
        -- forM_ (catMaybes xs) $ \(i,ref) -> do
        --     either 
        --         (fail . unpack) 
        --         (tell . Markdown . pack . [s|[table%d]: %s\n|] i . makeRelative cd) 
        --         ref
    where
        def =   mathComm' "dom"
             <> renewcommand "between" 3 (raw "#1 \\le #2 \\le #3")
             -- <> renewcommand "between" 3 (raw "#1 \\1\\le #2 \\1\\le #3")
             <> sys^.newCommands
        ps = ["multirow", "amsmath", "amssymb"]
 

tableImage :: Render t 
           => FilePath
           -> [Text]
           -> LaTeX
           -> FunctionTable t 
           -> IO (Either Text FilePath)
tableImage fn ps h t = mkPNG (do 
                    math .= False 
                    out  .= Just fn
                    bg   .= AlphaTransparent
                    pkgs .= ps
                    tightness .= True ) $
        -- render h <> "\\begin{align*} x + y \\\\ y - x \\end{align*}"
        render h <> render t
 
instance DocFormat Markdown where
    renderDoc (Title lvl t) = Markdown $ "\n" <> T.replicate (lvl + 1) "#" <> pack (" " ++ t ++ "\n")
    renderDoc (Ct t) = Markdown $ T.unlines $ L.map (uncurry $ (<>) . pack) $ contentToMD t
        where
            contentToMD (Line "") = [("","")]
            contentToMD (Line ln) = L.map (((,) "") . pack) $ L.lines ln
            contentToMD (Bold ct) = concatMap contentToMD ct &~ do
                                        _head._2 %= ("**" <>)
                                        _last._2 %= (<> "**")
            contentToMD (Italics ct) = concatMap contentToMD ct &~ do
                                        _head._2 %= ("_" <>)
                                        _last._2 %= (<> "_")
            contentToMD (StrikeThrough ct) = concatMap contentToMD ct &~ do
                                        _head._2 %= ("~~" <>)
                                        _last._2 %= (<> "~~")
            contentToMD (Item ls) = concat $ L.map (indentWith " * " "   " . contentToMD) ls
            contentToMD (Enum ls) = concat $ L.zipWith (\n -> indentWith' n . contentToMD) [1..] ls
                where
                    indentWith' n = indentWith x $ L.replicate (length x) ' '
                        where
                            x = " " ++ show n ++ ". "
            contentToMD (Image tag fn) = between "![" "  " (contentToMD tag) (pack $ [s|](%s)|] fn)
            contentToMD (Link tag fn)  = between "[" " " (contentToMD tag) (pack $ [s|](%s)|] fn)
            contentToMD (Seq x y)  = contentToMD x <> contentToMD y
            contentToMD Nil  = mempty
            contentToMD (DocTable t) = mkRow (heading t) : sep : L.map mkRow (rows t)
                where
                    cell (FormatCell _ xs) = xs
                    sep = ("",pack $ intercalate " | " $ L.replicate (columns t) "---")
                    mkRow xs = ("",pack $ "| " ++ intercalate " | " (L.map cell xs) ++ " |")
            contentToMD (Verbatim lang xs) = L.map (((,) "") . pack) $ ["```" ++ lang'] ++ L.lines xs ++ ["```"]
                where
                    lang' = maybe "" id lang
    -- func = 

between :: (Monoid a,Monoid b) => a -> a -> [(a,b)] -> b -> [(a,b)]
between x x' xs y 
        | null xs   = [(x,y)]
        | otherwise = xs &~ do
                _Cons %= bimap (first (x <>)) (L.map $ first (x' <>))
                _last._2 %= (<> y)
indentWith :: Monoid a => a -> a -> [(a,b)] -> [(a,b)]
indentWith x y = _Cons %~ bimap (first (x <>)) (L.map $ first (y <>))
