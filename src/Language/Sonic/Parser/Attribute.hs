module Language.Sonic.Parser.Attribute
  ( attrSetParser
  , attrParser
  , attrValueParser
  , attrValueListParser
  )
where

import           Data.Functor                   ( void )
import           Data.Text                      ( unpack )
import           Control.Applicative            ( Alternative(..) )
import           Control.Applicative.Combinators
                                                ( sepBy )

import           Text.Megaparsec                ( takeWhile1P
                                                , single
                                                , (<?>)
                                                )

import           Language.Sonic.Parser.Internal.Location
                                                ( Offset
                                                , withOffset
                                                )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse )
import           Language.Sonic.Parser.Internal.Source
                                                ( Source
                                                , fromChunk
                                                )
import           Language.Sonic.Parser.Internal.Lexer
                                                ( symbol )
import           Language.Sonic.Parser.Name     ( entityNameParser
                                                , attrKeyNameParser
                                                )
import           Language.Sonic.Parser.Path     ( pathParser )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Attribute
                                                ( Attr(..)
                                                , AttrSet(..)
                                                , AttrValue(..)
                                                , AttrValueList(..)
                                                )

attrParser :: Source s => Parse s (Attr Offset)
attrParser = do
  key <- withOffset attrKeyNameParser
  value key <|> list key <|> record key <|> pure (Name key)
 where
  value k = do
    symbol "="
    v <- withOffset attrValueParser
    pure $ Value k v
  list k = List k <$> withOffset attrValueListParser
  record k = Record k <$> withOffset attrSetParser

attrValueListParser :: Source s => Parse s (AttrValueList Offset)
attrValueListParser = do
  symbol "("
  vs <- withOffset attrValueParser `sepBy` symbol ","
  symbol ")"
  pure . AttrValueList $ Sequence vs

attrSetParser :: Source s => Parse s (AttrSet Offset)
attrSetParser = do
  symbol "["
  vs <- withOffset attrParser `sepBy` symbol ","
  symbol "]"
  pure . AttrSet $ Sequence vs

attrValueParser :: Source s => Parse s (AttrValue Offset)
attrValueParser = string <|> path <?> "attr value"
 where
  string = do
    void $ single '\"'
    txt <- fromChunk <$> takeWhile1P (Just "string") (/= '\"')
    void $ single '\"'
    pure . TextValue $ read ("\"" ++ unpack txt ++ "\"")
  path = PathValue <$> pathParser entityNameParser
