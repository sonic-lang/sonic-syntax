{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Sonic.Parser.LiteralSpec
  ( test_integer
  , test_char
  )
where

import           TestImport

import           Language.Sonic.Syntax.Literal

test_integer :: [TestTree]
test_integer =
  [ testCase "simple integer" $ assertParse "43" (Integer 43)
  , testCase "big integer" $ assertParse bigNumberText (Integer bigNumber)
  ]
 where
  bigNumber     = toInteger (maxBound :: Int) + 1
  bigNumberText = show bigNumber

test_char :: [TestTree]
test_char =
  [ testCase "simple char" $ assertParse "'c'" (Char 'c')
  , testCase "newline char" $ assertParse "'\\n'" (Char '\n')
  , testCase "escaped (dec) char" $ assertParse "'\\100'" (Char 'd')
  , testCase "escaped (hex) char" $ assertParse "'\\x64'" (Char 'd')
  , testCase "emoji" $ assertParse "'😁'" (Char '😁')
  , testCase "fails without terminal '"
    $ assertParseFail @Literal "'c" (expect "'")
  ]
