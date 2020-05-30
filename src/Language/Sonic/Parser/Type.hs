{-# LANGUAGE NamedFieldPuns #-}

module Language.Sonic.Parser.Type
  ( typeParser
  , infixParser
  , tyVarBinderParser
  , contextParser
  , predicateParser
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Control.Applicative.Combinators
                                                ( sepBy
                                                , optional
                                                )

import           Text.Megaparsec                ( (<?>) )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source )
import           Language.Sonic.Parser.Internal.Location
                                                ( Offset
                                                , withOffset
                                                )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse )
import           Language.Sonic.Parser.Internal.Lexer
                                                ( symbol
                                                , word
                                                , parens
                                                , space
                                                )
import           Language.Sonic.Parser.Internal.Tuple
                                                ( tupleOrParensParser )
import           Language.Sonic.Parser.Internal.Operator
                                                ( Operators
                                                , withOperators
                                                , infixLeftOp
                                                , postfixOp
                                                )
import           Language.Sonic.Parser.Name     ( tyVarNameParser
                                                , tyCtorNameParser
                                                , symbolTyCtorNameParser
                                                , classNameParser
                                                )
import           Language.Sonic.Parser.Path     ( pathParser )
import           Language.Sonic.Parser.Kind     ( kindParser )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Type     ( Type(..)
                                                , TyVarBinder(..)
                                                , Infix(..)
                                                , Context(..)
                                                , Predicate(..)
                                                )

varTypeParser :: Source s => Parse s (Type Offset)
varTypeParser = Var <$> withOffset tyVarNameParser

ctorTypeParser :: Source s => Parse s (Type Offset)
ctorTypeParser = Ctor <$> withOffset (pathParser tyCtorNameParser)

tupleOrParensTypeParser :: Source s => Parse s (Type Offset)
tupleOrParensTypeParser = tupleOrParensParser Tuple typeParser

forallTypeParser :: Source s => Parse s (Type Offset)
forallTypeParser = do
  word "forall"
  vars <- Sequence <$> some (withOffset tyVarBinderParser)
  symbol "."
  context <- optional $ withOffset contextParser
  symbol "=>"
  type_ <- withOffset typeParser
  pure $ Forall vars context type_

atomTypeParser :: Source s => Parse s (Type Offset)
atomTypeParser =
  varTypeParser
    <|> ctorTypeParser
    <|> tupleOrParensTypeParser
    <|> forallTypeParser

infixParser :: Source s => Parse s (Infix Offset)
infixParser = Infix <$> pathParser symbolTyCtorNameParser

operators :: Source s => [Operators s Type]
operators =
  [ infixLeftOp (Apply <$ space)
  , postfixOp (flip Annotate <$ symbol "::" <*> withOffset kindParser)
  , infixLeftOp (flip InfixApply <$> withOffset infixParser)
  ]

typeParser :: Source s => Parse s (Type Offset)
typeParser = withOperators operators atomTypeParser <?> "type"

tyVarBinderParser :: Source s => Parse s (TyVarBinder Offset)
tyVarBinderParser = annot <|> noAnnot <?> "type variable binder"
 where
  annot = parens $ do
    var <- withOffset tyVarNameParser
    symbol "::"
    kind <- withOffset kindParser
    pure TyVarBinder { var, kind = Just kind }
  noAnnot = do
    var <- withOffset tyVarNameParser
    pure TyVarBinder { var, kind = Nothing }

contextParser :: Source s => Parse s (Context Offset)
contextParser = Context <$> withOffset (singleton <|> tuple) <?> "context"
 where
  singleton = do
    predicate <- withOffset predicateParser
    pure $ Sequence [predicate]
  tuple = do
    symbol "("
    preds <- withOffset predicateParser `sepBy` symbol ","
    symbol ")"
    pure $ Sequence preds

classPredicateParser :: Source s => Parse s (Predicate Offset)
classPredicateParser = do
  className <- withOffset (pathParser classNameParser)
  types     <- many $ withOffset typeParser
  pure . Class className $ Sequence types

equalityPredicateParser :: Source s => Parse s (Predicate Offset)
equalityPredicateParser = do
  l <- withOffset typeParser
  symbol "~"
  r <- withOffset typeParser
  pure $ Equality l r

predicateParser :: Source s => Parse s (Predicate Offset)
predicateParser =
  equalityPredicateParser <|> classPredicateParser <?> "predicate"
