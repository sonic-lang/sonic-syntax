{-# LANGUAGE TypeFamilies        #-}

module Language.Sonic.Parser.Internal.Source
  ( Source(..)
  , Chunk(..)
  , chunk
  )
where

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )

import           Text.Megaparsec                ( MonadParsec )
import qualified Text.Megaparsec               as Parsec
                                                ( Stream(..)
                                                , chunk
                                                )

class (Parsec.Stream s, Parsec.Token s ~ Char, Parsec.Tokens s ~ SourceChunk s, Chunk (SourceChunk s)) => Source s where
  type SourceChunk s

class Chunk s where
  toChunk :: Text -> s
  fromChunk :: s -> Text

instance Source Text where
  type SourceChunk Text = Text

instance Source String where
  type SourceChunk String = String

instance Chunk Text where
  toChunk   = id
  fromChunk = id

instance Chunk String where
  toChunk   = unpack
  fromChunk = pack

chunk :: (MonadParsec e s m, Source s) => String -> m (SourceChunk s)
chunk = Parsec.chunk . toChunk . pack
