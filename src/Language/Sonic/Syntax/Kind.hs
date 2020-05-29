module Language.Sonic.Syntax.Kind
  ( Kind(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Location ( Located )

data Kind l
  = Type
  | Arrow (Located l Kind) (Located l Kind)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)
