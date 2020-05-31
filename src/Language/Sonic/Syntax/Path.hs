module Language.Sonic.Syntax.Path
  ( Path(..)
  , PathPrefix(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Name     ( ModuleComponentName )
import           Language.Sonic.Syntax.Location ( Located )

data Path n l
  = Path
  { prefix     :: Maybe (Located l PathPrefix)
  , modulePath :: Maybe (Located l (Sequence ModuleComponentName))
  , name       :: Located l n
  }
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

data PathPrefix l
  = Dot
  | Dollar
  | Hash
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
