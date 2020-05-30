module Language.Sonic.Syntax.Pattern
  ( Pat(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Location ( Located )
import           Language.Sonic.Syntax.Literal  ( Literal )
import           Language.Sonic.Syntax.Path     ( Path )
import           Language.Sonic.Syntax.Name     ( VarName
                                                , CtorName
                                                )

data Pat l
  = Wildcard
  | Literal (Located l Literal)
  | Var (Located l VarName)
  | Tuple (Located l (Sequence Pat))
  | Ctor (Located l (Path CtorName)) (Sequence Pat l)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)
