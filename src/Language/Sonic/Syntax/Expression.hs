module Language.Sonic.Syntax.Expression
  ( Expr(..)
  , Infix(..)
  , LetDefn(..)
  , LetBinder(..)
  , CaseArm(..)
  , Guard(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Name     ( CtorName
                                                , VarName
                                                )
import           Language.Sonic.Syntax.Location ( L )
import           Language.Sonic.Syntax.Path     ( Path )
import           Language.Sonic.Syntax.Literal  ( Literal )
import           Language.Sonic.Syntax.Type     ( Type )
import           Language.Sonic.Syntax.Pattern  ( Pat )

data Expr
  = Var (L (Path VarName))
  | Ctor (L (Path CtorName))
  | Literal (L Literal)
  | Tuple (L [L Expr])
  | Apply (L Expr) (L Expr)
  | InfixApply (L Expr) (L Infix) (L Expr)
  | Lambda (L VarName) (L Expr)
  | Annotate (L Expr) (L Type)
  | Let (L [L LetDefn]) (L Expr)
  | Case (L Expr) (L [L CaseArm])
  deriving (Show, Eq, Ord, Data, Generic)

data Infix
  = RawVar (Path VarName)
  | RawCtor (Path CtorName)
  | Quoted (L Expr)
  deriving (Show, Eq, Ord, Data, Generic)

data LetDefn
  = LetDefn
  { binder  :: L LetBinder
  , body    :: L Expr
  }
  deriving (Show, Eq, Ord, Data, Generic)

data LetBinder
  = LetBinder
  { name  :: L VarName
  , type_ :: Maybe (L Type)
  }
  deriving (Show, Eq, Ord, Data, Generic)

data CaseArm
  = CaseArm
  { pat   :: L Pat
  , guard :: L Guard
  , body  :: L Expr
  }
  deriving (Show, Eq, Ord, Data, Generic)

newtype Guard = Guard (L Expr)
  deriving (Show, Eq, Ord, Data, Generic)
