module Language.Sonic.Parser.Internal.Comment
  ( Comment(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text )

import           Language.Sonic.Parser.Internal.Location
                                                ( Position )

data Comment
  = Comment
  { isMultiLine :: Bool
  , content     :: Text
  , location    :: (Position, Position)
  }
  deriving (Show, Eq, Ord, Data, Generic)
