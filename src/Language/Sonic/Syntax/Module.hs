module Language.Sonic.Syntax.Module
  ( Module(..)
  , ModuleItem(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Location ( Located )
import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Declaration
                                                ( Decl )
import           Language.Sonic.Syntax.Attribute
                                                ( WithAttrSet
                                                , AttrSet
                                                )

data ModuleItem l
  = TopAttr (Located l AttrSet)
  | TopDecl (Located l (WithAttrSet Decl))
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype Module l = Module (Sequence ModuleItem l)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
