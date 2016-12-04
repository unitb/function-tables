{-# LANGUAGE TemplateHaskell #-}
module UnitB.FunctionTable.Spec.Doc where

import Control.Monad.Writer
import Data.Functor.Identity
import Data.String

import GHC.Generics
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
        | Seq Content Content
        | Nil
        | Verbatim String
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

verbatim :: QuasiQuoter 
verbatim = QuasiQuoter
     { quoteExp = \s -> [| emitContent $ Verbatim s |] }
