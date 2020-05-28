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

import           Language.Sonic.Syntax.Name     ( TyVarName
                                                , TyCtorName
                                                , ClassName
                                                )
import           Language.Sonic.Syntax.Location ( L )
import           Language.Sonic.Syntax.Path     ( Path )
import           Language.Sonic.Syntax.Kind     ( Kind )

data Type
  = Var (L (Path TyVarName))
  | Ctor (L (Path TyCtorName))
  | Tuple (L [L Type])
  | Apply (L Type) (L Type)
  | InfixApply (L Type) (L Infix) (L Type)
  | Annotate (L Type) (L Kind)
  | Forall [L TyVarBinder] (L Context) (L Type)
  deriving (Show, Eq, Ord, Data, Generic)

newtype Infix = Infix (Path TyCtorName)
  deriving (Show, Eq, Ord, Data, Generic)

data TyVarBinder
  = TyVarBinder
  { var  :: L TyVarName
  , kind :: Maybe (L Kind)
  }
  deriving (Show, Eq, Ord, Data, Generic)

newtype Context = Context (L [L Predicate])
  deriving (Show, Eq, Ord, Data, Generic)

data Predicate
  = Class (L (Path ClassName)) [L Type]
  | Equality (L Type) (L Type)
  deriving (Show, Eq, Ord, Data, Generic)
