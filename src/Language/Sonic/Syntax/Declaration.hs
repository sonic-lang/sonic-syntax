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
import           Data.List.NonEmpty             ( NonEmpty )

import           Language.Sonic.Syntax.Name     ( CtorName
                                                , VarName
                                                , TyCtorName
                                                , ClassName
                                                )
import           Language.Sonic.Syntax.Location ( L )
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
data Decl
  = Simple SimpleDecl
  | Data DataDecl
  | Class ClassDecl
  | Instance InstanceDecl
  deriving (Show, Eq, Ord, Data, Generic)

-- | Simple declaration, which is either a name signature declaration or a value declaration.
data SimpleDecl
  = Signature (SignatureDecl VarName)
  | Value ValueDecl
  | Function FunctionDecl
  deriving (Show, Eq, Ord, Data, Generic)

-- | Name signature declaration.
data SignatureDecl name
  = SignatureDecl
  { name  :: L name
  , type_ :: L Type
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Value declaration.
data ValueDecl
  = ValueDecl
  { pat      :: L Pat
  , body     :: L Expr
  , bindings :: Maybe (L (WhereClause SimpleDecl))
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Function declaration.
data FunctionDecl
  = FunctionDecl
  { name    :: L VarName
  , clauses :: L (NonEmpty (L FunctionClause))
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Clause in function declaration.
data FunctionClause
  = FunctionClause
  { pats     :: NonEmpty (L Pat)
  , guard    :: Maybe (L Guard)
  , body     :: L Expr
  , bindings :: Maybe (L (WhereClause SimpleDecl))
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Data type declaration.
data DataDecl
  = DataDecl
  { dataName  :: L TyCtorName
  , vars      :: [L TyVarBinder]
  , ctors     :: Maybe (L (WhereClause (SignatureDecl CtorName)))
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Type class declaration.
data ClassDecl
  = ClassDecl
  { context   :: Maybe (L Context)
  , className :: L ClassName
  , vars      :: [L TyVarBinder]
  , methods   :: Maybe (L (WhereClause SimpleDecl))
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Type class instance declaration.
data InstanceDecl
  = InstanceDecl
  { context   :: Maybe (L Context)
  , className :: L (Path ClassName)
  , types     :: [L Type]
  , methods   :: Maybe (L (WhereClause SimpleDecl))
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Where clause.
newtype WhereClause d = WhereClause (L [L d])
  deriving (Show, Eq, Ord, Data, Generic)
