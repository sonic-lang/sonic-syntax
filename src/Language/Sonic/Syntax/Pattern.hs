module Language.Sonic.Syntax.Pattern
  ( Pat(..)
  , PatInfix(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Location ( Located )
import           Language.Sonic.Syntax.Literal  ( Literal )
import           Language.Sonic.Syntax.Path     ( Path )
import           Language.Sonic.Syntax.Name     ( VarName
                                                , CtorName
                                                )

data Pat l
  = Parens (Located l Pat)
  | Wildcard
  | Literal (Located l Literal)
  | Var (Located l VarName)
  | Tuple (Located l (Sequence Pat))
  | Ctor (Located l (Path CtorName)) (Sequence Pat l)
  | Infix (Located l Pat) (Located l PatInfix) (Located l Pat)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

newtype PatInfix l = PatInfix (Path CtorName l)
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)
