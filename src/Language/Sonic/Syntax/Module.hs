module Language.Sonic.Syntax.Module
  ( Module(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Declaration
                                                ( Decl )

newtype Module l = Module (Sequence Decl l)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
