module Language.Sonic.Syntax.Type
  ( Type(..)
  , TypeInfix(..)
  , TyVarBinder(..)
  , Context(..)
  , Predicate(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
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
  = Var (Located l TyVarName)
  | Ctor (Located l (Path TyCtorName))
  | Tuple (Located l (Sequence Type))
  | Apply (Located l Type) (Located l Type)
  | InfixApply (Located l Type) (Located l TypeInfix) (Located l Type)
  | Annotate (Located l Type) (Located l Kind)
  | Forall (Sequence TyVarBinder l) (Maybe (Located l Context)) (Located l Type)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype TypeInfix l = TypeInfix (Path TyCtorName l)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

data TyVarBinder l
  = TyVarBinder
  { var  :: Located l TyVarName
  , kind :: Maybe (Located l Kind)
  }
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype Context l = Context (Located l (Sequence Predicate))
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

data Predicate l
  = Class (Located l (Path ClassName)) (Sequence Type l)
  | Equality (Located l Type) (Located l Type)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
