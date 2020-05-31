module Language.Sonic.Syntax.Kind
  ( Kind(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Location ( Located )

data Kind l
  = Type
  | Arrow (Located l Kind) (Located l Kind)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
