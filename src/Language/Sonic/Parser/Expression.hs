{-# LANGUAGE NamedFieldPuns #-}

module Language.Sonic.Parser.Expression
  ( exprParser
  , exprInfixParser
  , letDefnParser
  , letBinderParser
  , caseArmParser
  , guardParser
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Control.Applicative.Combinators
                                                ( sepBy1
                                                , sepEndBy
                                                , optional
                                                )

import           Text.Megaparsec                ( try
                                                , (<?>)
                                                )

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
import           Language.Sonic.Parser.Name     ( varNameParser
                                                , ctorNameParser
                                                , symbolVarNameParser
                                                , symbolCtorNameParser
                                                )
import           Language.Sonic.Parser.Path     ( pathParser )
import           Language.Sonic.Parser.Literal  ( literalParser )
import           Language.Sonic.Parser.Type     ( typeParser )
import           Language.Sonic.Parser.Pattern  ( patParser
                                                , atomPatParser
                                                )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Expression
                                                ( Expr(..)
                                                , ExprInfix(..)
                                                , LetDefn(..)
                                                , LetBinder(..)
                                                , CaseArm(..)
                                                , Guard(..)
                                                )

varExprParser :: Source s => Parse s (Expr Offset)
varExprParser = Var <$> withOffset (pathParser varNameParser)

ctorExprParser :: Source s => Parse s (Expr Offset)
ctorExprParser = Ctor <$> withOffset (pathParser ctorNameParser)

literalExprParser :: Source s => Parse s (Expr Offset)
literalExprParser = Literal <$> withOffset literalParser

tupleOrParensExprParser :: Source s => Parse s (Expr Offset)
tupleOrParensExprParser = tupleOrParensParser Tuple Parens exprParser

lambdaExprParser :: Source s => Parse s (Expr Offset)
lambdaExprParser = do
  symbol "\\"
  pats <- many (withOffset atomPatParser)
  symbol "."
  body <- withOffset exprParser
  pure $ Lambda (Sequence pats) body

letExprParser :: Source s => Parse s (Expr Offset)
letExprParser = do
  word "let"
  defns <- withOffset defnsParser
  word "in"
  body <- withOffset exprParser
  pure $ Let defns body
 where
  defnsParser = manyDefns <|> oneDefn
  oneDefn     = do
    defn <- withOffset letDefnParser
    pure $ Sequence [defn]
  manyDefns = do
    symbol "{"
    defns <- withOffset letDefnParser `sepBy1` symbol ","
    symbol "}"
    pure $ Sequence defns

caseExprParser :: Source s => Parse s (Expr Offset)
caseExprParser = do
  word "case"
  target <- withOffset exprParser
  arms   <- withOffset armsParser
  pure $ Case target arms
 where
  armsParser = do
    symbol "{"
    arms <- withOffset caseArmParser `sepEndBy` symbol ","
    symbol "}"
    pure $ Sequence arms

atomExprParser :: Source s => Parse s (Expr Offset)
atomExprParser =
  literalExprParser
    <|> tupleOrParensExprParser
    <|> try lambdaExprParser
    <|> letExprParser
    <|> caseExprParser
    <|> try varExprParser
    <|> ctorExprParser

operators :: Source s => [Operators s Expr]
operators =
  [ infixLeftOp (Apply <$ space)
  , infixLeftOp (flip InfixApply <$> withOffset exprInfixParser)
  , postfixOp (flip Annotate <$ symbol "::" <*> withOffset typeParser)
  ]

exprParser :: Source s => Parse s (Expr Offset)
exprParser = withOperators operators atomExprParser

exprInfixParser :: Source s => Parse s (ExprInfix Offset)
exprInfixParser =
  quoted
    <|> try (var symbolVarNameParser)
    <|> ctor symbolCtorNameParser
    <?> "infix operator"
 where
  quoted = do
    symbol "`"
    e <- try (var varNameParser) <|> ctor ctorNameParser
    symbol "`"
    pure e
  var n = VarInfix <$> pathParser n
  ctor n = CtorInfix <$> pathParser n

letDefnParser :: Source s => Parse s (LetDefn Offset)
letDefnParser = do
  binder <- withOffset letBinderParser
  symbol "="
  body <- withOffset exprParser
  pure LetDefn { binder, body }

letBinderParser :: Source s => Parse s (LetBinder Offset)
letBinderParser = try annotated <|> pat
 where
  pat       = PatBinder <$> withOffset patParser
  annotated = do
    name <- withOffset varNameParser
    symbol "::"
    type_ <- withOffset typeParser
    pure $ AnnotatedBinder name type_

caseArmParser :: Source s => Parse s (CaseArm Offset)
caseArmParser = do
  pat   <- withOffset patParser
  guard <- optional $ withOffset guardParser
  symbol "=>"
  body <- withOffset exprParser
  pure CaseArm { pat, guard, body }

guardParser :: Source s => Parse s (Guard Offset)
guardParser = do
  word "if"
  e <- withOffset exprParser
  pure $ Guard e
