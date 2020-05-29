module Language.Sonic.Syntax.Type
  ( Type(..)
  , Infix(..)
  , TyVarBinder(..)
  , Context(..)
  , Predicate(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Name     ( TyVarName
                                                , TyCtorName
                                                , ClassName
                                                )
import           Language.Sonic.Syntax.Location ( Located )
import           Language.Sonic.Syntax.Path     ( Path )
import           Language.Sonic.Syntax.Kind     ( Kind )

data Type l
  = Var (Located l (Path TyVarName))
  | Ctor (Located l (Path TyCtorName))
  | Tuple (Located l (Sequence Type))
  | Apply (Located l Type) (Located l Type)
  | InfixApply (Located l Type) (Located l Infix) (Located l Type)
  | Annotate (Located l Type) (Located l Kind)
  | Forall (Sequence TyVarBinder l) (Located l Context) (Located l Type)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

newtype Infix l = Infix (Path TyCtorName l)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data TyVarBinder l
  = TyVarBinder
  { var  :: Located l TyVarName
  , kind :: Maybe (Located l Kind)
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

newtype Context l = Context (Located l (Sequence Predicate))
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data Predicate l
  = Class (Located l (Path ClassName)) [Located l Type]
  | Equality (Located l Type) (Located l Type)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)
