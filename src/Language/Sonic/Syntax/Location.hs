module Language.Sonic.Syntax.Location
  ( Located
  , L(..)
  , noLoc
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

type Located l a = L a l

data L a l
  = L
  { begin   :: l
  , content :: a l
  , end     :: l
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

noLoc :: Located l a -> a l
noLoc (L _ a _) = a
