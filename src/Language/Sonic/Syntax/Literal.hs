module Language.Sonic.Syntax.Literal
  ( Literal(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

data Literal
  = Integer Integer
  | Boolean Bool
  deriving (Show, Eq, Ord, Data, Generic)
