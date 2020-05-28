module Language.Sonic.Syntax.Module
  ( Module(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Location ( L )
import           Language.Sonic.Syntax.Declaration
                                                ( Decl )
import           Language.Sonic.Syntax.Comment  ( Comment )

data Module
  = Module
  { decls    :: [L Decl]
  , comments :: [L Comment]
  }
  deriving (Show, Eq, Ord, Data, Generic)
