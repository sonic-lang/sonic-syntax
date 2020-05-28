module Language.Sonic.Syntax.Path
  ( Path(..)
  , PathPrefix(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Name     ( ModuleComponentName )
import           Language.Sonic.Syntax.Location ( L )

data Path n
  = Path
  { prefix     :: Maybe (L PathPrefix)
  , modulePath :: L [L ModuleComponentName]
  , name       :: L n
  }
  deriving (Show, Eq, Ord, Data, Generic)

data PathPrefix
  = Dot
  | Colon
  | Hash
  deriving (Show, Eq, Ord, Data, Generic)
