module Language.Sonic.Syntax.Sequence
  ( Sequence(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Location ( Located )

-- | (possibly punctuated) sequence of @a@
newtype Sequence a l = Sequence [Located l a]
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
