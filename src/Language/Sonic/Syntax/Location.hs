module Language.Sonic.Syntax.Location
  ( L
  , Located(..)
  , Location(..)
  , Range(..)
  , Position(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Numeric.Natural                ( Natural )

type L a = Located a

data Located a = Located Location a
  deriving (Show, Eq, Ord, Data, Generic)

data Location
  = Location
  { file  :: FilePath
  , range :: Range
  }
  deriving (Show, Eq, Ord, Data, Generic)

data Range
  = Range
  { begin :: Position
  , end   :: Position
  }
  deriving (Show, Eq, Ord, Data, Generic)

data Position
  = Position
  { line   :: Natural
  , column :: Natural
  }
  deriving (Show, Eq, Ord, Data, Generic)
