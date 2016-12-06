{-# LANGUAGE TemplateHaskell #-}
module UnitB.FunctionTable.Spec.Doc where

import Control.Lens
import Control.Monad.Writer
import Data.Bitraversable
import Data.Char
import Data.List.Lens
import Data.String hiding (lines)
import Data.String.Lines
import Data.String.Utils

import Prelude hiding (lines)

import GHC.Generics (Generic)
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote

data Doc = 
        Title Int String
        | Ct Content
    deriving (Eq,Ord,Show,Generic)

type URI = String

data Content = 
        Line String 
        | Item [Content] 
        | Enum [Content]
        | Image Content URI
        | Link  Content URI
        | Bold [Content]
        | Italics [Content]
        | StrikeThrough [Content]
        | Seq Content Content
        | Nil
        | Verbatim (Maybe String) String
    deriving (Eq,Ord,Show,Generic)

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
instance IsString Content where
    fromString = Line
instance Monoid Content where
    mempty = Nil
    mappend Nil x = x
    mappend x Nil = x
    mappend (Seq x y) z = Seq x $ y `mappend` z
    mappend x y = Seq x y

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

nest :: ([Content] -> Content)
     -> ContentWriter a
     -> ContentWriter a
nest f (ContentWriter cmd) = ContentWriter $ censor (pure . f) cmd

strike :: ContentWriter a -> ContentWriter a
strike = nest StrikeThrough

bold :: ContentWriter a -> ContentWriter a
bold = nest Bold

italics :: ContentWriter a -> ContentWriter a
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
     { quoteExp  = \str -> [e| bitraverse id id $(mkQuoted str) |]
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

