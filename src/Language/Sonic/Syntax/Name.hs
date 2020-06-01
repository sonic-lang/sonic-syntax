module Language.Sonic.Syntax.Name
  ( CtorName(..)
  , VarName(..)
  , TyCtorName(..)
  , TyVarName(..)
  , ClassName(..)
  , ModuleComponentName(..)
  , AttrKeyName(..)
  -- * Union of name variants
  , ValueName(..)
  , TypeName(..)
  , EntityName(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )

import           Language.Sonic.Syntax.Location ( Located )

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

newtype AttrKeyName l = AttrKeyName Text
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

-- | Name in value world.
data ValueName l
  = CtorValueName (CtorName l)
  | VarValueName (VarName l)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

-- | Name in type world.
data TypeName l
  = CtorTypeName (TyCtorName l)
  | VarTypeName (TyVarName l)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

-- | Name that can point any entity. Each variant of names is disambiguated with different syntax.
data EntityName l
  = ValueEntityName (Located l ValueName)
  | TypeEntityName (Located l TypeName)
  | ClassEntityName (Located l ClassName)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
