module Language.Sonic.Syntax.Literal
  ( Literal(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

data Literal l
  = Integer Integer
  | Char Char
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
