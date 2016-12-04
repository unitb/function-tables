{-# LANGUAGE OverloadedStrings,QuasiQuotes #-}
module UnitB.FunctionTable.Spec.MarkDown where

import Control.Lens
import Control.Monad.Writer
import Data.Maybe
import Data.Text (pack,unpack)

import TeX2PNG

import System.Directory
import System.FilePath

import Text.LaTeX
import Text.LaTeX.FunctionTable
import Text.Printf.TH

import UnitB.FunctionTable.Spec.LaTeX
import UnitB.FunctionTable.Spec.Types

specToMD :: TeXSpec -> IO Text
specToMD sys = execWriterT $ do
        xs <- forM (zip [0..] $ contents $ sys^.specs) $ \(i,ln) -> do
            case ln of
                Left  str   -> do
                    tell $ pack str <> "\n"
                    return Nothing
                Right (_b,tbl) -> do
                    r <- lift $ tableImage ([s|table%d|] i) ps def tbl
                    tell $ pack $ [s|![alt text][table%d] \n |] i
                    return $ Just (i,r)
        cd <- liftIO getCurrentDirectory
        forM_ (catMaybes xs) $ \(i,ref) -> do
            either 
                (fail . unpack) 
                (tell . pack . [s|[table%d]: %s\n|] i . makeRelative cd) 
                ref
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
                    bg   .= SimplyTransparent
                    pkgs .= ps
                    tightness .= True ) $
        -- render h <> "\\begin{align*} x + y \\\\ y - x \\end{align*}"
        render h <> render t
