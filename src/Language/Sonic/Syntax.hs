module Language.Sonic.Syntax
  ( module X
  )
where

import           Language.Sonic.Syntax.Name    as X
                                                ( CtorName
                                                , VarName
                                                , TyCtorName
                                                , TyVarName
                                                , ClassName
                                                , ModuleComponentName
                                                , AttrKeyName
                                                , ValueName
                                                , TypeName
                                                , EntityName
                                                )
import           Language.Sonic.Syntax.Path    as X
                                                ( Path
                                                , PathPrefix
                                                )
import           Language.Sonic.Syntax.Literal as X
                                                ( Literal )
import           Language.Sonic.Syntax.Pattern as X
                                                ( Pat
                                                , PatInfix
                                                )
import           Language.Sonic.Syntax.Kind    as X
                                                ( Kind )
import           Language.Sonic.Syntax.Type    as X
                                                ( Type
                                                , TypeInfix
                                                , TyVarBinder
                                                , Context
                                                , Predicate
                                                )
import           Language.Sonic.Syntax.Expression
                                               as X
                                                ( Expr
                                                , ExprInfix
                                                , LetDefn
                                                , LetBinder
                                                , CaseArm
                                                , Guard
                                                )
import           Language.Sonic.Syntax.Declaration
                                               as X
                                                ( Decl
                                                , SimpleDecl
                                                , SignatureDecl
                                                , ValueDecl
                                                , FunctionDecl
                                                , FunctionClause
                                                , DataDecl
                                                , ClassDecl
                                                , InstanceDecl
                                                , WhereClause
                                                )
import           Language.Sonic.Syntax.Attribute
                                               as X
                                                ( WithAttrSet
                                                , AttrSet
                                                , Attr
                                                , AttrValue
                                                , AttrValueList
                                                )
import           Language.Sonic.Syntax.Module  as X
                                                ( Module
                                                , ModuleItem
                                                )
