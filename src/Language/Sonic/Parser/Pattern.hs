module Language.Sonic.Parser.Pattern
  ( patParser
  , atomPatParser
  , patInfixParser
  )
where

import           Control.Applicative            ( Alternative(..) )
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
                                                ( symbol )
import           Language.Sonic.Parser.Internal.Tuple
                                                ( tupleOrParensParser )
import           Language.Sonic.Parser.Internal.Operator
                                                ( Operators
                                                , withOperators
                                                , infixLeftOp
                                                )
import           Language.Sonic.Parser.Name     ( ctorNameParser
                                                , varNameParser
                                                , symbolCtorNameParser
                                                )
import           Language.Sonic.Parser.Path     ( pathParser )
import           Language.Sonic.Parser.Literal  ( literalParser )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Pattern  ( Pat(..)
                                                , PatInfix(..)
                                                )

wildcardPatParser :: Source s => Parse s (Pat Offset)
wildcardPatParser = Wildcard <$ symbol "_"

literalPatParser :: Source s => Parse s (Pat Offset)
literalPatParser = Literal <$> withOffset literalParser

varPatParser :: Source s => Parse s (Pat Offset)
varPatParser = Var <$> withOffset varNameParser

tupleOrParensPatParser :: Source s => Parse s (Pat Offset)
tupleOrParensPatParser = tupleOrParensParser Tuple Parens patParser

ctorPatParser :: Source s => Parse s (Pat Offset)
ctorPatParser = do
  path <- withOffset $ pathParser ctorNameParser
  args <- ctorArgsParser
  pure $ Ctor path args
  where ctorArgsParser = Sequence <$> many (withOffset atomPatParser)

nullaryCtorPatParser :: Source s => Parse s (Pat Offset)
nullaryCtorPatParser = do
  path <- withOffset $ pathParser ctorNameParser
  pure . Ctor path $ Sequence []

atomPatParser :: Source s => Parse s (Pat Offset)
atomPatParser =
  wildcardPatParser
    <|> literalPatParser
    <|> tupleOrParensPatParser
    <|> try nullaryCtorPatParser
    <|> varPatParser

operators :: Source s => [Operators s Pat]
operators = [infixLeftOp (flip Infix <$> withOffset patInfixParser)]

patInfixParser :: Source s => Parse s (PatInfix Offset)
patInfixParser = PatInfix <$> pathParser symbolCtorNameParser

patParser :: Source s => Parse s (Pat Offset)
patParser =
  withOperators operators (try ctorPatParser <|> atomPatParser) <?> "pattern"
