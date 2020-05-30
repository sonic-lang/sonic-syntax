module Language.Sonic.Parser.Internal.Tuple
  ( tupleOrParensParser
  )
where

import           Control.Applicative.Combinators
                                                ( sepBy )

import           Language.Sonic.Parser.Internal.Location
                                                ( Offset
                                                , withOffset
                                                )
import           Language.Sonic.Parser.Internal.Source
                                                ( Source )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse )
import           Language.Sonic.Parser.Internal.Lexer
                                                ( symbol )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Location ( noLoc
                                                , Located
                                                )

-- | @'tupleOrParensParser' ctor p@ parses a tuple of @p@ constructed with @ctor@, or @p@ with parentheses.
tupleOrParensParser
  :: Source s
  => (Located Offset (Sequence a) -> a Offset)
  -> Parse s (a Offset)
  -> Parse s (a Offset)
tupleOrParensParser ctor p = do
  xs <- withOffset tupleParser
  pure $ case noLoc xs of
    Sequence [x] -> noLoc x
    _            -> ctor xs
 where
  tupleParser = do
    symbol "("
    xs <- withOffset p `sepBy` symbol ","
    symbol ")"
    pure $ Sequence xs
