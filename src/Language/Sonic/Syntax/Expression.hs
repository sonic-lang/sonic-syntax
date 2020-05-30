module Language.Sonic.Syntax.Expression
  ( Expr(..)
  , ExprInfix(..)
  , LetDefn(..)
  , LetBinder(..)
  , CaseArm(..)
  , Guard(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Name     ( CtorName
                                                , VarName
                                                )
import           Language.Sonic.Syntax.Location ( Located )
import           Language.Sonic.Syntax.Path     ( Path )
import           Language.Sonic.Syntax.Literal  ( Literal )
import           Language.Sonic.Syntax.Type     ( Type )
import           Language.Sonic.Syntax.Pattern  ( Pat )

data Expr l
  = Var (Located l (Path VarName))
  | Ctor (Located l (Path CtorName))
  | Literal (Located l Literal)
  | Tuple (Located l (Sequence Expr))
  | Apply (Located l Expr) (Located l Expr)
  | InfixApply (Located l Expr) (Located l ExprInfix) (Located l Expr)
  | Lambda (Sequence Pat l) (Located l Expr)
  | Annotate (Located l Expr) (Located l Type)
  | Let (Located l (Sequence LetDefn)) (Located l Expr)
  | Case (Located l Expr) (Located l (Sequence CaseArm))
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data ExprInfix l
  = RawVar (Path VarName l)
  | RawCtor (Path CtorName l)
  | Quoted (Located l Expr)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data LetDefn l
  = LetDefn
  { binder  :: Located l LetBinder
  , body    :: Located l Expr
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data LetBinder l
  = PatBinder (Located l Pat)
  | AnnotatedBinder (Located l VarName) (Located l Type)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data CaseArm l
  = CaseArm
  { pat   :: Located l Pat
  , guard :: Maybe (Located l Guard)
  , body  :: Located l Expr
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

newtype Guard l = Guard (Located l Expr)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)
