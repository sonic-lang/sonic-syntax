module Language.Sonic.Parser
  ( parse
  , parseNonGreedy
  , parseWithComment
  , parseNonGreedyWithComment
  -- * Parser class
  , Parsable
  , Symbol(..)
  -- * Source
  , Source
  , Comment(..)
  , Position(..)
  -- * Errors
  , Error(..)
  , UnexpectedTokenError(..)
  )
where

import           Control.Monad.Except           ( MonadError )

import           Text.Megaparsec                ( eof )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source )
import           Language.Sonic.Parser.Internal.Location
                                                ( Position(..)
                                                , withOffset
                                                )
import           Language.Sonic.Parser.Internal.Error
                                                ( Error(..)
                                                , UnexpectedTokenError(..)
                                                )
import           Language.Sonic.Parser.Internal.Parse
                                                ( runParse )
import           Language.Sonic.Parser.Internal.Lexer
                                                ( space )
import           Language.Sonic.Parser.Internal.Comment
                                                ( Comment(..) )
import           Language.Sonic.Parser.Parsable ( Parsable(..)
                                                , Symbol(..)
                                                )
import           Language.Sonic.Syntax.Location ( Located )

parse
  :: (Parsable a, Source s, MonadError (Error s) m)
  => s
  -> m (Located Position a)
parse = fmap fst . parseWithComment

parseWithComment
  :: (Parsable a, Source s, MonadError (Error s) m)
  => s
  -> m (Located Position a, [Comment])
parseWithComment input = do
  (a, comments, _) <- runParse (space *> withOffset parser <* eof) input
  pure (a, comments)

parseNonGreedy
  :: (Parsable a, Source s, MonadError (Error s) m)
  => s
  -> m (Located Position a, s)
parseNonGreedy input = do
  (a, _, rest) <- parseNonGreedyWithComment input
  pure (a, rest)

parseNonGreedyWithComment
  :: (Parsable a, Source s, MonadError (Error s) m)
  => s
  -> m (Located Position a, [Comment], s)
parseNonGreedyWithComment = runParse (space *> withOffset parser)