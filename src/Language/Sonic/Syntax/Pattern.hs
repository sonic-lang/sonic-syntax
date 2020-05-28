module Language.Sonic.Syntax.Pattern
  ( Pat(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Location ( L )
import           Language.Sonic.Syntax.Literal  ( Literal )
import           Language.Sonic.Syntax.Name     ( VarName
                                                , CtorName
                                                )

data Pat
  = Wildcard
  | Literal (L Literal)
  | Var (L VarName)
  | Tuple (L [L Pat])
  | Ctor (L CtorName) [L Pat]
  deriving (Show, Eq, Ord, Data, Generic)
