module Language.Sonic.Syntax.Name
  ( CtorName(..)
  , VarName(..)
  , TyCtorName(..)
  , TyVarName(..)
  , ClassName(..)
  , ModuleComponentName(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )

newtype CtorName = CtorName Text
  deriving (Show, Eq, Ord, Data, Generic)

newtype VarName = VarName Text
  deriving (Show, Eq, Ord, Data, Generic)

newtype TyCtorName = TyCtorName Text
  deriving (Show, Eq, Ord, Data, Generic)

newtype TyVarName = TyVarName Text
  deriving (Show, Eq, Ord, Data, Generic)

newtype ClassName = ClassName Text
  deriving (Show, Eq, Ord, Data, Generic)

newtype ModuleComponentName = ModuleComponentName Text
  deriving (Show, Eq, Ord, Data, Generic)
