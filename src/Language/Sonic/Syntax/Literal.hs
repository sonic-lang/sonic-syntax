module Language.Sonic.Syntax.Literal
  ( Literal(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

data Literal l
  = Integer Integer
  | Char Char
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)
