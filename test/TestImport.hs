{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module TestImport
  ( assertParse
  , assertParseFail
  , ParseErrorPredicate
  , at
  , expect
  , get
  , Position(..)
  , TokenItem(..)
  , loc
  , path
  , Sequence(..)
  , Symbol(..)
  , module Tasty
  )
where

import           Data.Functor                   ( ($>) )
import           Data.Semigroup                 ( Last(..) )
import           Data.Text                      ( pack )
import           Numeric.Natural                ( Natural )
import           GHC.Exts                       ( IsString(..) )

import           Test.Tasty                    as Tasty
import           Test.Tasty.HUnit              as Tasty

import           Language.Sonic.Syntax.Location ( Located
                                                , L(..)
                                                , noLoc
                                                )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Path     ( Path(..) )
import           Language.Sonic.Parser          ( Parsable
                                                , Source
                                                , Symbol(..)
                                                , parse
                                                , Position(..)
                                                , Error(..)
                                                , TokenItem(..)
                                                , UnexpectedTokenError(..)
                                                )

loc :: a () -> Located () a
loc x = L () x ()

path :: n () -> Path n ()
path = Path Nothing Nothing . loc

assertParse
  :: (Parsable a, Eq (a ()), Show (a ())) => String -> a () -> Assertion
assertParse input expected = case parse input of
  Left err -> assertFailure $ show err
  Right got ->
    assertEqual ("while parsing " ++ show input) expected (noLoc (got $> ()))

data ParseErrorPredicate
  = ParseErrorPredicate
  { positionP :: Maybe (Last Position)
  , expectedP :: [TokenItem String]
  , foundP    :: Maybe (Last (TokenItem String))
  }
  deriving Show

instance Semigroup ParseErrorPredicate where
  ParseErrorPredicate p1 e1 f1 <> ParseErrorPredicate p2 e2 f2 =
    ParseErrorPredicate (p1 <> p2) (e1 <> e2) (f1 <> f2)

instance Monoid ParseErrorPredicate where
  mempty = ParseErrorPredicate Nothing [] Nothing

at :: Natural -> Natural -> ParseErrorPredicate
at line column = mempty { positionP = Just $ Last Position { line, column } }

expect :: TokenItem String -> ParseErrorPredicate
expect item = mempty { expectedP = [item] }

get :: TokenItem String -> ParseErrorPredicate
get item = mempty { foundP = Just $ Last item }

-- | Orphan instance to omit 'Chunk' in @expect $ Chunk "str"@
instance Source s => IsString (TokenItem s) where
  fromString = ChunkItem . pack

match :: ParseErrorPredicate -> UnexpectedTokenError String -> Bool
match ParseErrorPredicate { positionP, expectedP, foundP } UnexpectedTokenError { position, expected, found }
  = matchPosition positionP && matchExpected expectedP && matchFound foundP
 where
  matchPosition (Just (Last p)) = p == position
  matchPosition Nothing         = True
  matchExpected es = all (`elem` expected) es
  matchFound (Just (Last item)) = Just item == found
  matchFound Nothing            = True

assertParseFail
  :: forall a
   . (Parsable a, Show (a Position))
  => String
  -> ParseErrorPredicate
  -> Assertion
assertParseFail input predicate = case parse @a input of
  Right x ->
    assertFailure
      $  "parsing "
      ++ show input
      ++ " succeeds as "
      ++ show x
      ++ " while it is expected to fail"
  Left (UnexpectedToken es) -> assertBool
    (  "unexpected parse error; expected: "
    ++ show predicate
    ++ " but got: "
    ++ show es
    )
    (any (match predicate) es)
