module Language.Sonic.Parser.Pattern
  ( patParser
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Control.Applicative.Combinators
                                                ( sepBy )
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
                                                ( symbol )
import           Language.Sonic.Parser.Name     ( ctorNameParser
                                                , varNameParser
                                                )
import           Language.Sonic.Parser.Path     ( pathParser )
import           Language.Sonic.Parser.Literal  ( literalParser )
import           Language.Sonic.Syntax.Location ( noLoc )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Pattern  ( Pat(..) )

wildcardPatParser :: Source s => Parse s (Pat Offset)
wildcardPatParser = Wildcard <$ symbol "_"

literalPatParser :: Source s => Parse s (Pat Offset)
literalPatParser = Literal <$> withOffset literalParser

varPatParser :: Source s => Parse s (Pat Offset)
varPatParser = Var <$> withOffset varNameParser

tupleOrParensPatParser :: Source s => Parse s (Pat Offset)
tupleOrParensPatParser = do
  pats <- withOffset tuplePatsParser
  pure $ case noLoc pats of
    Sequence [x] -> noLoc x
    _            -> Tuple pats
 where
  tuplePatsParser = do
    symbol "("
    pats <- withOffset patParser `sepBy` symbol ","
    symbol ")"
    pure $ Sequence pats

ctorPatParser :: Source s => Parse s (Pat Offset)
ctorPatParser = do
  path <- withOffset $ pathParser ctorNameParser
  args <- ctorArgsParser
  pure $ Ctor path args
  where ctorArgsParser = Sequence <$> many (withOffset patParser)

patParser :: Source s => Parse s (Pat Offset)
patParser =
  wildcardPatParser
    <|> literalPatParser
    <|> varPatParser
    <|> tupleOrParensPatParser
    <|> ctorPatParser
    <?> "pattern"
