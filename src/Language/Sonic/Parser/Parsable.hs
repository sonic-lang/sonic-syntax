module Language.Sonic.Parser.Parsable
  ( Parsable(..)
  , Symbol(..)
  )
where

import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Data.Data                      ( Data )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source )
import           Language.Sonic.Parser.Internal.Location
                                                ( Offset )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse )

import           Language.Sonic.Parser.Name
import           Language.Sonic.Parser.Path
import           Language.Sonic.Parser.Literal
import           Language.Sonic.Parser.Pattern
import           Language.Sonic.Parser.Kind
import           Language.Sonic.Parser.Type
import           Language.Sonic.Parser.Expression
import           Language.Sonic.Parser.Attribute
import           Language.Sonic.Parser.Declaration
import           Language.Sonic.Parser.Module

import           Language.Sonic.Syntax.Name
import           Language.Sonic.Syntax.Path
import           Language.Sonic.Syntax.Literal
import           Language.Sonic.Syntax.Pattern
import           Language.Sonic.Syntax.Kind
import           Language.Sonic.Syntax.Type
import           Language.Sonic.Syntax.Expression
import           Language.Sonic.Syntax.Attribute
import           Language.Sonic.Syntax.Declaration
import           Language.Sonic.Syntax.Module

class Traversable a => Parsable a where
  parser :: Source s => Parse s (a Offset)

-- | Wrapper to parse names as infix symbols
newtype Symbol a l = Symbol { unSymbol :: a l }
  deriving (Show, Eq, Data, Generic, Generic1, Functor, Foldable, Traversable)

-- Name

instance Parsable CtorName where
  parser = ctorNameParser

instance Parsable (Symbol CtorName) where
  parser = Symbol <$> symbolCtorNameParser

instance Parsable VarName where
  parser = varNameParser

instance Parsable (Symbol VarName) where
  parser = Symbol <$> symbolVarNameParser

instance Parsable TyCtorName where
  parser = tyCtorNameParser

instance Parsable (Symbol TyCtorName) where
  parser = Symbol <$> symbolTyCtorNameParser

instance Parsable TyVarName where
  parser = tyVarNameParser

instance Parsable ClassName where
  parser = classNameParser

instance Parsable ModuleComponentName where
  parser = moduleComponentNameParser

instance Parsable AttrKeyName where
  parser = attrKeyNameParser

instance Parsable ValueName where
  parser = valueNameParser

instance Parsable TypeName where
  parser = typeNameParser

instance Parsable EntityName where
  parser = entityNameParser

-- Path

instance Parsable PathPrefix where
  parser = pathPrefixParser

instance Parsable n => Parsable (Path n) where
  parser = pathParser parser

-- Literal

instance Parsable Literal where
  parser = literalParser

-- Pattern

instance Parsable Pat where
  parser = patParser

instance Parsable PatInfix where
  parser = patInfixParser

-- Kind

instance Parsable Kind where
  parser = kindParser

-- Type

instance Parsable Type where
  parser = typeParser

instance Parsable TypeInfix where
  parser = typeInfixParser

instance Parsable TyVarBinder where
  parser = tyVarBinderParser

instance Parsable Context where
  parser = contextParser

instance Parsable Predicate where
  parser = predicateParser

-- Expression

instance Parsable Expr where
  parser = exprParser

instance Parsable ExprInfix where
  parser = exprInfixParser

instance Parsable LetDefn where
  parser = letDefnParser

instance Parsable LetBinder where
  parser = letBinderParser

instance Parsable CaseArm where
  parser = caseArmParser

instance Parsable Guard where
  parser = guardParser

-- Attr

instance Parsable AttrValue where
  parser = attrValueParser

instance Parsable AttrValueList where
  parser = attrValueListParser

instance Parsable Attr where
  parser = attrParser

instance Parsable AttrSet where
  parser = attrSetParser

-- Declaration

instance Parsable Decl where
  parser = declParser

instance Parsable SimpleDecl where
  parser = simpleDeclParser

instance Parsable DataDecl where
  parser = dataDeclParser

instance Parsable ClassDecl where
  parser = classDeclParser

instance Parsable InstanceDecl where
  parser = instanceDeclParser

instance Parsable n => Parsable (SignatureDecl n) where
  parser = signatureDeclParser parser

instance Parsable ValueDecl where
  parser = valueDeclParser

instance Parsable FunctionDecl where
  parser = functionDeclParser

instance Parsable FunctionClause where
  parser = functionClauseParser

instance Parsable d => Parsable (WhereClause d) where
  parser = whereClauseParser parser

-- Module

instance Parsable Module where
  parser = moduleParser
