module Language.Sonic.Parser.Kind
  ( kindParser
  )
where

import           Control.Applicative            ( Alternative(..) )
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
                                                )
import           Language.Sonic.Syntax.Location ( noLoc )
import           Language.Sonic.Syntax.Kind     ( Kind(..) )

typeKindParser :: Source s => Parse s (Kind Offset)
typeKindParser = Type <$ word "Type"

atomKindParser :: Source s => Parse s (Kind Offset)
atomKindParser = parens kindParser <|> typeKindParser

kindParser :: Source s => Parse s (Kind Offset)
kindParser = do
  l <- withOffset atomKindParser
  arrowRight l <|> pure (noLoc l) <?> "kind"
 where
  arrowRight l = do
    symbol "->"
    r <- withOffset kindParser
    pure $ Arrow l r
