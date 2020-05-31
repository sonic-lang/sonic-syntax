{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns        #-}

module Language.Sonic.Parser.Internal.Error
  ( Error(..)
  , UnexpectedTokenError(..)
  , TokenItem(ChunkItem, LabelItem, EndItem)
  , toErrorItem
  , fromErrorItem
  , fromParseErrorBundle
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Void                      ( Void )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.List.NonEmpty             ( NonEmpty
                                                , toList
                                                , fromList
                                                )
import qualified Data.Set                      as Set
                                                ( toList )

import qualified Text.Megaparsec               as Parsec
                                                ( ErrorItem(..)
                                                , ParseError(..)
                                                , SourcePos
                                                , ParseErrorBundle(..)
                                                , Token
                                                , tokensToChunk
                                                , chunkToTokens
                                                , attachSourcePos
                                                , errorOffset
                                                )

import           Language.Sonic.Parser.Internal.Location
                                                ( Position
                                                , fromSourcePos
                                                )
import           Language.Sonic.Parser.Internal.Source
                                                ( Source(..)
                                                , toChunk
                                                , fromChunk
                                                )

data TokenItem s
  = Chunk (SourceChunk s)
  | Label Text
  | EndOfInput

deriving instance Source s => Eq (TokenItem s)
deriving instance Source s => Ord (TokenItem s)

instance Source s => Show (TokenItem s) where
  show (Chunk c)  = "(Chunk " ++ show (fromChunk c) ++ ")"
  show (Label t)  = "(Label " ++ show t ++ ")"
  show EndOfInput = "EndOfInput"

pattern ChunkItem :: Source s => Text -> TokenItem s
pattern ChunkItem c <- (Chunk (fromChunk -> c))
  where
    ChunkItem c = Chunk (toChunk c)

pattern LabelItem :: Text -> TokenItem s
pattern LabelItem t = Label t

pattern EndItem :: TokenItem s
pattern EndItem = EndOfInput

data UnexpectedTokenError s
  = UnexpectedTokenError
  { position :: Position
  , expected :: [TokenItem s]
  , found    :: Maybe (TokenItem s)
  }
  deriving (Show, Eq, Ord, Generic)

data Error s = UnexpectedToken (NonEmpty (UnexpectedTokenError s))
  deriving (Show, Eq, Ord, Generic)

fromErrorItem
  :: forall s . Source s => Parsec.ErrorItem (Parsec.Token s) -> TokenItem s
fromErrorItem (Parsec.Tokens ts) =
  Chunk . Parsec.tokensToChunk (Proxy @s) $ toList ts
fromErrorItem (Parsec.Label cs) = Label . pack $ toList cs
fromErrorItem Parsec.EndOfInput = EndOfInput

toErrorItem
  :: forall s . Source s => TokenItem s -> Parsec.ErrorItem (Parsec.Token s)
toErrorItem (Chunk chunk) =
  Parsec.Tokens . fromList $ Parsec.chunkToTokens (Proxy @s) chunk
toErrorItem (Label txt) = Parsec.Label . fromList $ unpack txt
toErrorItem EndOfInput  = Parsec.EndOfInput

fromParseError
  :: Source s
  => (Parsec.ParseError s Void, Parsec.SourcePos)
  -> UnexpectedTokenError s
fromParseError (Parsec.TrivialError _ mfound expectedSet, pos) =
  UnexpectedTokenError { position, expected, found }
 where
  found    = fmap fromErrorItem mfound
  expected = map fromErrorItem $ Set.toList expectedSet
  position = fromSourcePos pos
fromParseError _ = error "unreachable: unused error"

fromParseErrorBundle :: Source s => Parsec.ParseErrorBundle s Void -> Error s
fromParseErrorBundle Parsec.ParseErrorBundle { Parsec.bundleErrors = errors, Parsec.bundlePosState = posState }
  = UnexpectedToken $ fmap fromParseError errorsWithPos
 where
  (errorsWithPos, _) =
    Parsec.attachSourcePos Parsec.errorOffset errors posState
