module Language.Sonic.Syntax.Declaration
  ( Decl(..)
  , SimpleDecl(..)
  , SignatureDecl(..)
  , ValueDecl(..)
  , FunctionDecl(..)
  , FunctionClause(..)
  , DataDecl(..)
  , ClassDecl(..)
  , InstanceDecl(..)
  , WhereClause(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )

import           Language.Sonic.Syntax.Sequence ( Sequence )
import           Language.Sonic.Syntax.Name     ( CtorName
                                                , VarName
                                                , TyCtorName
                                                , ClassName
                                                )
import           Language.Sonic.Syntax.Location ( Located )
import           Language.Sonic.Syntax.Path     ( Path )
import           Language.Sonic.Syntax.Type     ( Context
                                                , Type
                                                , TyVarBinder
                                                )
import           Language.Sonic.Syntax.Pattern  ( Pat )
import           Language.Sonic.Syntax.Expression
                                                ( Expr
                                                , Guard
                                                )

-- | Top-level declaration.
data Decl l
  = Simple (SimpleDecl l)
  | Data (DataDecl l)
  | Class (ClassDecl l)
  | Instance (InstanceDecl l)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

-- | Simple declaration, which is either a name signature declaration or a value declaration.
data SimpleDecl l
  = Signature (SignatureDecl VarName l)
  | Value (ValueDecl l)
  | Function (FunctionDecl l)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

-- | Name signature declaration.
data SignatureDecl name l
  = SignatureDecl
  { name  :: Located l name
  , type_ :: Located l Type
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

-- | Value declaration.
data ValueDecl l
  = ValueDecl
  { pat      :: Located l Pat
  , body     :: Located l Expr
  , bindings :: Maybe (Located l (WhereClause SimpleDecl))
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

-- | Function declaration.
data FunctionDecl l
  = FunctionDecl
  { name    :: Located l VarName
  , clauses :: Located l (Sequence FunctionClause)
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

-- | Clause in function declaration.
data FunctionClause l
  = FunctionClause
  { pats     :: Sequence Pat l
  , guard    :: Maybe (Located l Guard)
  , body     :: Located l Expr
  , bindings :: Maybe (Located l (WhereClause SimpleDecl))
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

-- | Data type declaration.
data DataDecl l
  = DataDecl
  { dataName  :: Located l TyCtorName
  , vars      :: Sequence TyVarBinder l
  , ctors     :: Maybe (Located l (WhereClause (SignatureDecl CtorName)))
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

-- | Type class declaration.
data ClassDecl l
  = ClassDecl
  { context   :: Maybe (Located l Context)
  , className :: Located l ClassName
  , vars      :: Sequence TyVarBinder l
  , methods   :: Maybe (Located l (WhereClause SimpleDecl))
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

-- | Type class instance declaration.
data InstanceDecl l
  = InstanceDecl
  { context   :: Maybe (Located l Context)
  , className :: Located l (Path ClassName)
  , types     :: Sequence Type l
  , methods   :: Maybe (Located l (WhereClause SimpleDecl))
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

-- | Where clause.
newtype WhereClause d l = WhereClause (Located l (Sequence d))
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)
