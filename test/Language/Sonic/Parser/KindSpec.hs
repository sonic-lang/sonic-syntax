module Language.Sonic.Parser.KindSpec
  ( test_kind
  )
where

import           TestImport

import           Language.Sonic.Syntax.Kind

test_kind :: [TestTree]
test_kind =
  [ testCase "Type kind" $ assertParse "Type" Type
  , testCase "arrow kind"
    $ assertParse "Type -> Type" (Arrow (loc Type) (loc Type))
  , testCase "arrow kind is right associative" $ assertParse
    "Type -> Type -> Type"
    (Arrow (loc Type) (loc (Arrow (loc Type) (loc Type))))
  , testCase "arrow kind with parentheses" $ assertParse
    "(Type -> Type) -> Type"
    (Arrow (loc (Arrow (loc Type) (loc Type))) (loc Type))
  ]
