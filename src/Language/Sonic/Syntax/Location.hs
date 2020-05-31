module Language.Sonic.Syntax.Location
  ( Located
  , L(..)
  , noLoc
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

type Located l a = L a l

data L a l
  = L
  { begin   :: l
  , content :: a l
  , end     :: l
  }
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

noLoc :: Located l a -> a l
noLoc (L _ a _) = a
