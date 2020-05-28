module Language.Sonic.Syntax.Comment
  ( Comment(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )

data Comment
  = MultiLine Text
  | SingleLine Text
  deriving (Show, Eq, Ord, Data, Generic)
