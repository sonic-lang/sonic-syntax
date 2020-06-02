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

-- | @'tupleOrParensParser' tuple parens p@ parses a tuple of @p@ constructed with @tuple@, or @p@ with parentheses constructed with @parens@.
tupleOrParensParser
  :: Source s
  => (Located Offset (Sequence a) -> a Offset)
  -> (Located Offset a -> a Offset)
  -> Parse s (a Offset)
  -> Parse s (a Offset)
tupleOrParensParser tupleCtor parensCtor p = do
  xs <- withOffset tupleParser
  pure $ case noLoc xs of
    Sequence [x] -> parensCtor x
    _            -> tupleCtor xs
 where
  tupleParser = do
    symbol "("
    xs <- withOffset p `sepBy` symbol ","
    symbol ")"
    pure $ Sequence xs
