{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Sonic.Parser.PathSpec
  ( test_pathPrefix
  , test_path
  )
where

import           TestImport

import           Language.Sonic.Syntax.Path
import           Language.Sonic.Syntax.Name

test_pathPrefix :: [TestTree]
test_pathPrefix =
  [ testCase ". is path prefix" $ assertParse "." Dot
  , testCase "$ is path prefix" $ assertParse "$" Dollar
  , testCase "# is path prefix" $ assertParse "#" Hash
  , testCase ": is not path prefix" $ assertParseFail @PathPrefix ":" mempty
  ]

test_path :: [TestTree]
test_path =
  [ testCase "variable name"
    $ assertParse "var" (Path Nothing Nothing (loc (VarName "var")))
  , testCase "constructor name"
    $ assertParse "Ctor" (Path Nothing Nothing (loc (CtorName "Ctor")))
  , testCase "variable name path" $ assertParse
    "std.print"
    (Path Nothing
          (Just (loc (Sequence [loc (ModuleComponentName "std")])))
          (loc (VarName "print"))
    )
  , testCase "variable name path with prefix" $ assertParse
    "$std.print"
    (Path (Just (loc Dollar))
          (Just (loc (Sequence [loc (ModuleComponentName "std")])))
          (loc (VarName "print"))
    )
  , testCase "variable name with prefix"
    $ assertParse ".x" (Path (Just (loc Dot)) Nothing (loc (VarName "x")))
  , testCase "operator symbol name"
    $ assertParse "+" (Path Nothing Nothing (loc (Symbol (VarName "+"))))
  , testCase "operator symbol name path" $ assertParse
    "std.num.+"
    (Path
      Nothing
      (Just
        (loc
          (Sequence
            [loc (ModuleComponentName "std"), loc (ModuleComponentName "num")]
          )
        )
      )
      (loc (Symbol (VarName "+")))
    )
  ]
