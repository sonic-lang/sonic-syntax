module Language.Sonic.Syntax.Path
  ( Path(..)
  , PathPrefix(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Name     ( ModuleComponentName )
import           Language.Sonic.Syntax.Location ( Located )

data Path n l
  = Path
  { prefix     :: Maybe (Located l PathPrefix)
  , modulePath :: Located l (Sequence ModuleComponentName)
  , name       :: Located l n
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data PathPrefix l
  = Dot
  | Colon
  | Hash
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)
