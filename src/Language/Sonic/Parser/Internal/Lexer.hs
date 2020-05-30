{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Sonic.Parser.Internal.Lexer
  ( space
  , lexeme
  , symbol
  , word
  , parens
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Functor                   ( void )
import qualified Data.Char                     as Char
                                                ( isSpace
                                                , isAlphaNum
                                                )
import           Control.Applicative.Combinators
                                                ( between )

import qualified Text.Megaparsec.Char.Lexer    as L
                                                ( space
                                                , lexeme
                                                )
import qualified Text.Megaparsec               as Parsec
                                                ( tokensToChunk )
import           Text.Megaparsec                ( takeWhileP
                                                , takeWhile1P
                                                , manyTill
                                                , anySingle
                                                , notFollowedBy
                                                , satisfy
                                                )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source(..)
                                                , Chunk(..)
                                                , chunk
                                                )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse
                                                , addComment
                                                )
import           Language.Sonic.Parser.Internal.Location
                                                ( getPosition )
import           Language.Sonic.Parser.Internal.Comment
                                                ( Comment(..) )

space :: Source s => Parse s ()
space = L.space space1 line block
 where
  space1 = void $ takeWhile1P (Just "whitespace") Char.isSpace
  line   = addComment =<< lineCommentParser
  block  = addComment =<< blockCommentParser

lexeme :: Source s => Parse s a -> Parse s a
lexeme = L.lexeme space

symbol :: Source s => String -> Parse s ()
symbol = void . lexeme . chunk

word :: Source s => String -> Parse s ()
word w = lexeme (chunk w *> notFollowedBy identChar)
 where
  identChar = satisfy isIdentChar
  isIdentChar c = Char.isAlphaNum c || c == '_'

parens :: Source s => Parse s a -> Parse s a
parens = between (symbol "(") (symbol ")")

lineCommentParser :: Source s => Parse s Comment
lineCommentParser = do
  begin   <- getPosition
  comment <- chunk "--" *> takeWhileP (Just "comment") (/= '\n')
  end     <- getPosition
  pure Comment { isMultiLine = False
               , content     = fromChunk comment
               , location    = (begin, end)
               }

blockCommentParser :: forall s . Source s => Parse s Comment
blockCommentParser = do
  begin   <- getPosition
  comment <- chunk "{-" *> manyTill anySingle (chunk "-}")
  end     <- getPosition
  let chunkContent = Parsec.tokensToChunk (Proxy @s) comment
  pure Comment { isMultiLine = True
               , content     = fromChunk chunkContent
               , location    = (begin, end)
               }
