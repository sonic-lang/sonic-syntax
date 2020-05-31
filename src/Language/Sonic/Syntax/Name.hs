module Language.Sonic.Syntax.Name
  ( CtorName(..)
  , VarName(..)
  , TyCtorName(..)
  , TyVarName(..)
  , ClassName(..)
  , ModuleComponentName(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )

newtype CtorName l = CtorName Text
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype VarName l = VarName Text
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype TyCtorName l = TyCtorName Text
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype TyVarName l = TyVarName Text
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype ClassName l = ClassName Text
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype ModuleComponentName l = ModuleComponentName Text
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
