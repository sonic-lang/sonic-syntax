module Language.Sonic.Syntax.Kind
  ( Kind(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Location ( L )

data Kind
  = Type
  | Arrow (L Kind) (L Kind)
  deriving (Show, Eq, Ord, Data, Generic)
