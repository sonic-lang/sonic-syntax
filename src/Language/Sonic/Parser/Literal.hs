module Language.Sonic.Parser.Literal
  ( literalParser
  )
where

import qualified Data.Char                     as Char
                                                ( readLitChar
                                                , isDigit
                                                )
import           Data.Text                      ( unpack )
import           Data.Functor                   ( void )
import           Control.Applicative            ( Alternative(..) )

import           Text.Megaparsec                ( takeWhile1P
                                                , single
                                                )

import           Language.Sonic.Parser.Internal.Location
                                                ( Offset )
import           Language.Sonic.Parser.Internal.Error
                                                ( chunkItem
                                                , labelItem
                                                )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse
                                                , unexpectedToken
                                                )
import           Language.Sonic.Parser.Internal.Source
                                                ( Source
                                                , fromChunk
                                                )
import           Language.Sonic.Parser.Internal.Lexer
                                                ( lexeme )
import           Language.Sonic.Syntax.Literal  ( Literal(..) )

integer :: Source s => Parse s Integer
integer = read . unpack . fromChunk <$> takeWhile1P (Just "digit") Char.isDigit

char :: Source s => Parse s Char
char = do
  void $ single '\''
  cs <- unpack . fromChunk <$> takeWhile1P (Just "char") (/= '\'')
  case Char.readLitChar cs of
    (c, ""   ) : _ -> pure c
    (_, h : _) : _ -> unexpectedToken h (chunkItem "'")
    []             -> unexpectedToken (head cs) (labelItem "valid char")

literalParser :: Source s => Parse s (Literal Offset)
literalParser = Char <$> lexeme char <|> Integer <$> lexeme integer
