module Language.Sonic.Syntax.Attribute
  ( AttrSet(..)
  , Attr(..)
  , AttrValue(..)
  , AttrValueList(..)
  , attrKey
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )

import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Name     ( AttrKeyName
                                                , EntityName
                                                )
import           Language.Sonic.Syntax.Path     ( Path )
import           Language.Sonic.Syntax.Location ( Located )

newtype AttrSet l = AttrSet (Sequence Attr l)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype AttrValueList l = AttrValueList (Sequence AttrValue l)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

data Attr l
  = Name (Located l AttrKeyName)
  | Value (Located l AttrKeyName) (Located l AttrValue)
  | List (Located l AttrKeyName) (Located l AttrValueList)
  | Record (Located l AttrKeyName) (Located l AttrSet)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

attrKey :: Attr l -> Located l AttrKeyName
attrKey (Name k    ) = k
attrKey (Value  k _) = k
attrKey (List   k _) = k
attrKey (Record k _) = k

data AttrValue l
  = TextValue Text
  | PathValue (Path EntityName l)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
