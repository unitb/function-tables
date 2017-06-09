{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module UnitB.FunctionTable.Spec.Types where

import Control.Lens
import Control.Lens.Internal.Setter

import Data.Map as M

import GHC.Generics.Instances

import Logic.Expr as N hiding (array)

import Text.LaTeX as T hiding (tex,(&))
import Text.LaTeX.FunctionTable as T

import UnitB.FunctionTable.Spec.Doc

type Primed = Bool
type SpecE = Spec (Map Expr) Expr VarDeclE Def
type TeXSpec a = Spec (SpecContent a) LaTeXLI VarDeclT LaTeXLI
newtype SpecContent b a = Content { _contents :: [Either (b -> [Doc]) a] }
    deriving (Functor,Foldable,Traversable,Generic)
data VarDeclT = VarDeclT (Type -> Type) LaTeXLI Primed
data VarDeclE = VarDeclE Var (Maybe Expr)

type Assumed = Bool

data Spec t a var def = Spec 
        { _newCommands :: LaTeX  
        , specPrefix  :: LaTeX 
        , _sortDecl   :: Map Name Sort
        , _dataCons   :: Map Name Def
        , _userDef    :: Map Name def
        , _userConst  :: Map Name var
        , _specs :: t (Assumed,FunctionTable a) }
    deriving (Generic)

makeLenses ''Spec
makeLenses ''SpecContent

argument :: Setter (TeXSpec a) (TeXSpec b) b a
argument f = specs.contents.traverse._Left $ taintedDot (lmap $ untaintedDot f)

class HasDecl decl v where
    decl :: Lens' decl v

instance HasDecl VarDeclT LaTeXLI where
    decl f (VarDeclT g d p) = (\d' -> VarDeclT g d' p) <$> f d
instance HasDecl VarDeclE Var where
    decl f (VarDeclE d wd) = (\d' -> VarDeclE d' wd) <$> f d

wd :: VarDeclE -> Maybe Expr
wd (VarDeclE _ d) = d

instance Monoid (SpecContent a b) where
    mappend = genericMAppend
    mempty  = genericMEmpty
instance t ~ SpecContent b => Monoid (Spec t a var def) where
    mappend = genericMAppend
    mempty  = genericMEmpty
instance Profunctor SpecContent where
    dimap f g = contents.mapped %~ bimap (lmap f) g
