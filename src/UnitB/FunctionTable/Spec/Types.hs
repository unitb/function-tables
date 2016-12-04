{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module UnitB.FunctionTable.Spec.Types where

import Control.Lens

import Data.Map as M

import GHC.Generics.Instances

import Logic.Expr as N hiding (array)

import Text.LaTeX as T hiding (tex,(&))
import Text.LaTeX.FunctionTable as T

type Primed = Bool
type SpecE = Spec (Map Expr) Expr VarDeclE Def
type TeXSpec = Spec Content LaTeXLI VarDeclT LaTeXLI
newtype Content a = Content { contents :: [Either String a] }
    deriving (Functor,Foldable,Traversable,Generic)
data VarDeclT = VarDeclT (Type -> Type) LaTeXLI Primed
data VarDeclE = VarDeclE Var (Maybe Expr)

data Spec t a var def = Spec 
        { _newCommands :: LaTeX  
        , specPrefix  :: LaTeX 
        , _sortDecl   :: Map Name Sort
        , _dataCons   :: Map Name Def
        , _userDef    :: Map Name def
        , _userConst  :: Map Name var
        , _specs :: t (Bool,FunctionTable a) }
    deriving (Generic)

makeLenses ''Spec

class HasDecl decl v where
    decl :: Lens' decl v

instance HasDecl VarDeclT LaTeXLI where
    decl f (VarDeclT g d p) = (\d' -> VarDeclT g d' p) <$> f d
instance HasDecl VarDeclE Var where
    decl f (VarDeclE d wd) = (\d' -> VarDeclE d' wd) <$> f d

wd :: VarDeclE -> Maybe Expr
wd (VarDeclE _ d) = d

instance Monoid (Content a) where
    mappend = genericMAppend
    mempty  = genericMEmpty
instance t ~ Content => Monoid (Spec t a var def) where
    mappend = genericMAppend
    mempty  = genericMEmpty
