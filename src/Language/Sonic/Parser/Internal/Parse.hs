{-# LANGUAGE NamedFieldPuns #-}

module Language.Sonic.Parser.Internal.Parse
  ( Parse
  , addComment
  , unexpectedToken
  , unexpectedChunk
  , matchToken
  , runParse
  )
where

import           Data.Void                      ( Void )
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , fromList
                                                )
import qualified Data.Set                      as Set
                                                ( singleton
                                                , fromList
                                                )
import           Data.Text                      ( unpack )
import           Control.Monad                  ( MonadPlus )
import           Control.Applicative            ( Alternative )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Strict     ( StateT
                                                , modify
                                                , runStateT
                                                )

import           Text.Megaparsec                ( Parsec
                                                , MonadParsec
                                                )
import qualified Text.Megaparsec               as Parsec
                                                ( State(..)
                                                , PosState(..)
                                                , ErrorItem(..)
                                                , initialPos
                                                , defaultTabWidth
                                                , failure
                                                , token
                                                , runParser'
                                                )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source(..)
                                                , fromChunk
                                                )
import           Language.Sonic.Parser.Internal.Error
                                                ( Error
                                                , TokenItem
                                                , toErrorItem
                                                , fromParseErrorBundle
                                                )
import           Language.Sonic.Parser.Internal.Comment
                                                ( Comment )
import           Language.Sonic.Parser.Internal.Location
                                                ( Offset
                                                , Position
                                                , attachPosition
                                                )
import           Language.Sonic.Syntax.Location ( Located )

newtype Parse s a = Parse { unParse :: StateT [Comment] (Parsec Void s) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadParsec Void s)

addComment :: Source s => Comment -> Parse s ()
addComment c = Parse $ modify (c :)

unexpectedToken :: Source s => Char -> TokenItem s -> Parse s a
unexpectedToken found = unexpected unexpectedItem
  where unexpectedItem = Parsec.Tokens (found :| [])

unexpectedChunk :: Source s => SourceChunk s -> TokenItem s -> Parse s a
unexpectedChunk found = unexpected unexpectedItem
  where unexpectedItem = Parsec.Tokens . fromList . unpack $ fromChunk found

unexpected :: Source s => Parsec.ErrorItem Char -> TokenItem s -> Parse s a
unexpected found expected = Parse $ lift failure
 where
  failure       = Parsec.failure (Just found) expectedItems
  expectedItems = Set.singleton $ toErrorItem expected

matchToken :: Source s => (Char -> Maybe a) -> [TokenItem s] -> Parse s a
matchToken f expected = Parsec.token f expectedItems
  where expectedItems = Set.fromList $ map toErrorItem expected

runParse
  :: (Source s, MonadError (Error s) m, Traversable a)
  => Parse s (Located Offset a)
  -> s
  -> m (Located Position a, s, [Comment])
runParse parser content = case Parsec.runParser' parsec initState of
  (_, Left bundle) -> throwError $ fromParseErrorBundle bundle
  (Parsec.State { Parsec.stateInput }, Right (a, comments)) ->
    pure (attachPosition initPosState a, stateInput, comments)
 where
  parsec    = runStateT (unParse parser) []
  initState = Parsec.State { stateInput       = content
                           , stateOffset      = 0
                           , statePosState    = initPosState
                           , stateParseErrors = []
                           }
  initPosState = Parsec.PosState
    { pstateInput      = content
    , pstateOffset     = 0
    , pstateSourcePos  = Parsec.initialPos dummyFile
    , pstateTabWidth   = Parsec.defaultTabWidth
    , pstateLinePrefix = ""
    }
  dummyFile = "<input>"  -- filename information is discarded anyway
