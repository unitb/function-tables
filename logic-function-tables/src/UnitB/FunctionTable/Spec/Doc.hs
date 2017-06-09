{-# LANGUAGE TemplateHaskell
        ,ExistentialQuantification
        ,PolyKinds
        ,DataKinds #-}
module UnitB.FunctionTable.Spec.Doc where

import Control.Lens
import Control.Lens.Bound
import Control.Monad.Writer hiding (All)
import Data.Bitraversable
import Data.Char
import Data.Constraint
import Data.Existential
import Data.List.Lens
import Data.String hiding (lines)
import Data.String.Lines
import Data.String.Utils
import Data.Vector.Sized (Vector,toList)
-- import Data.Vector.Sized.Quote 
import Data.Type.Natural
import Data.Typeable

import Prelude hiding (lines)

import GHC.Generics (Generic)
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote

newtype Table a = Tbl { _tableCell :: Cell1 (TableIntl a) All }
    deriving (Generic)

data TableIntl a n = TblIntl (Dict (SingI n,Eq a)) (Vector a n) [Vector a n]
    deriving (Eq,Ord,Show,Generic)

makeFields ''Table

data Doc = 
        Title Int String
        | Ct Content
    deriving (Eq,Ord,Show,Generic)

type URI = String

data Color = Red | Yellow | Green
    deriving (Eq,Ord,Show,Generic,Enum,Bounded)

data FormatCell = FormatCell (Maybe Color) String
    deriving (Eq,Ord,Show,Generic)

data Content = 
        Line String 
        | Item [Content] 
        | Enum [Content]
        | Image Content URI
        | Link  Content URI
        | Bold [Content]
        | Italics [Content]
        | StrikeThrough [Content]
        | DocTable (Table FormatCell)
        | Seq Content Content
        | Nil
        | Verbatim (Maybe String) String
    deriving (Eq,Ord,Show,Generic)

instance (Typeable a,Eq a) => Eq (Table a) where
    (==) = cell1Equal' (==)
instance (Typeable a,Ord a) => Ord (Table a) where
    compare = cell1Compare' compare
instance Show a => Show (Table a) where
    show = readCell1' show

class Monoid out => DocFormat out where
    renderDoc :: Doc -> out

class Monad m => DocBuilder m where
    emitContent :: Content -> m ()

runDoc :: DocFormat out => Writer [Doc] k -> out
runDoc = runIdentity . runDocT

runDocT :: (Monad m,DocFormat out) => WriterT [Doc] m k -> m out
runDocT = fmap (mconcat . fmap renderDoc) . execWriterT

instance IsString Doc where
    fromString = Ct . Line
instance IsString FormatCell where
    fromString = FormatCell Nothing
instance IsString Content where
    fromString = Line
instance Monoid Content where
    mempty = Nil
    mappend Nil x = x
    mappend x Nil = x
    mappend (Seq x y) z = Seq x $ y `mappend` z
    mappend x y = Seq x y

toInt :: Dict (SingI n, Eq a) -> SNat n -> Int
toInt _ = sNatToInt

columns :: Table a -> Int
columns = readCell1' columns'

columns' :: TableIntl a n -> Int
columns' (TblIntl d@Dict _ _) = toInt d sing

heading :: Table t -> [t]
heading (Tbl (Cell (TblIntl _ t _))) = toList t

rows :: Table t -> [[t]]
rows (Tbl (Cell (TblIntl _ _ ts))) = map toList ts

makeDocTable :: (DocBuilder m,SingI n,Typeable n)
             => Vector FormatCell n -> [Vector FormatCell n] -> m ()
makeDocTable h ts = emitContent $ docTable h ts

docTable :: (SingI n,Typeable n)
         => Vector FormatCell n -> [Vector FormatCell n] -> Content
docTable h ts = DocTable . makeCell1 $ TblIntl Dict h ts

text :: DocBuilder m => String -> m ()
text = emitContent . Line

paragraph :: DocBuilder m => m a -> m a
paragraph txt = text "\n" >> txt <* text "\n"

newtype ListEnv a = ListEnv (Writer [Content] a)
    deriving (Functor,Applicative,Monad)

listBullet :: DocBuilder m => ListEnv a -> m a
listBullet (ListEnv cmd) = do
        let (x,is) = runWriter cmd
        emitContent $ Item is
        return x

listNum :: DocBuilder m => ListEnv a -> m a
listNum (ListEnv cmd) = do
        let (x,is) = runWriter cmd
        emitContent $ Enum is
        return x

newtype ContentWriter a = ContentWriter (Writer [Content] a)
    deriving (Functor,Applicative,Monad)

instance DocBuilder ContentWriter where
    emitContent = ContentWriter . tell . pure

instance a ~ () => IsString (ContentWriter a) where
    fromString = ContentWriter . tell . pure . Line

execContentWriter :: ContentWriter a -> [Content]
execContentWriter (ContentWriter cmd) = execWriter cmd

item :: ContentWriter a -> ListEnv a
item (ContentWriter cmd) = do
        let (x,is) = runWriter cmd
        ListEnv $ tell [mconcat is]
        return x

image :: DocBuilder m
      => ContentWriter a 
      -> FilePath 
      -> m a
image (ContentWriter cmd) lnk = do
        let (x,is) = runWriter cmd
        emitContent $ Image (mconcat is) lnk
        return x

link :: DocBuilder m
     => ContentWriter a 
     -> FilePath 
     -> m a
link (ContentWriter cmd) lnk = do
        let (x,is) = runWriter cmd
        emitContent $ Link (mconcat is) lnk
        return x

nest :: DocBuilder m
     => ([Content] -> Content)
     -> ContentWriter a
     -> m a
nest f (ContentWriter cmd) = emitContent (f w) >> return x
    where
        (x,w) = runWriter cmd

strike :: DocBuilder m
       => ContentWriter a -> m a
strike = nest StrikeThrough

bold :: DocBuilder m
     => ContentWriter a -> m a
bold = nest Bold

italics :: DocBuilder m
        => ContentWriter a -> m a
italics = nest Italics

trimLines :: String -> String
trimLines xs 
        | Just n' <- n = xs & traverseLines %~ drop n'
        | otherwise = xs
    where
        n = minimumOf (traverse.filtered (not . all isSpace).to (length.takeWhile (' ' ==))) $ lines xs

verbatim :: QuasiQuoter 
verbatim = QuasiQuoter
     { quoteExp  = \s -> [| emitContent $ Verbatim Nothing $ trimLines s |] 
     , quoteDec  = undefined 
     , quoteType = undefined 
     , quotePat  = undefined }

quoteSyntax :: String -> ExpQ
quoteSyntax xs 
        | Just s' <- s^?prefixed "|" = [| emitContent $ Verbatim (Just lang) $ trimLines s' |] 
        | otherwise                  = error "invalid syntax: expecting '|'"
    where
        (lang,s) = span (/= '|') xs

syntax :: QuasiQuoter 
syntax = QuasiQuoter
     { quoteExp  = quoteSyntax
     , quoteDec  = undefined 
     , quoteType = undefined 
     , quotePat  = undefined }

mkQuoted :: String -> ExpQ
mkQuoted str = case parseExp str' of
           Left msg -> fail $ "Could not parse expression. " ++ msg
           Right exp -> [e|
                ( $(pure exp) 
                , emitContent (Verbatim (Just "haskell") $(stringE $ trimLines str'))) |] 
    where
        str' = replace "\\]" "|]" str

quoted :: QuasiQuoter
quoted = QuasiQuoter
     { quoteExp  = \str -> [e| void $ bitraverse id id $(mkQuoted str) |]
     , quoteDec  = undefined 
     , quoteType = undefined 
     , quotePat  = undefined }

listing :: QuasiQuoter
listing = QuasiQuoter
     { quoteExp  = \str -> [e| fst <$> bitraverse return id $(mkQuoted str) |]
     , quoteDec  = undefined 
     , quoteType = undefined 
     , quotePat  = undefined }

exec :: QuasiQuoter
exec = QuasiQuoter
     { quoteExp  = \str -> [e| snd <$> bitraverse id return $(mkQuoted str) |]
     , quoteDec  = undefined 
     , quoteType = undefined 
     , quotePat  = undefined }

