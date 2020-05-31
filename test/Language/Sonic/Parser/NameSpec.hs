{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Sonic.Parser.NameSpec
  ( test_ctorName
  , test_symbolCtorName
  , test_varName
  , test_symbolVarName
  , test_tyCtorName
  , test_symbolTyCtorName
  , test_tyVarName
  , test_className
  , test_moduleComponentName
  )
where

import           TestImport

import           Language.Sonic.Syntax.Name

test_ctorName :: [TestTree]
test_ctorName =
  [ testCase "constructor name" $ assertParse "Ctor" (CtorName "Ctor")
  , testCase "constructor name accept numbers"
    $ assertParse "Ctor1x" (CtorName "Ctor1x")
  , testCase "constructor name does not accept underscore"
    $ assertParseFail @CtorName "Ctor_name" (get "_")
  , testCase "constructor name does not start with lowercase"
    $ assertParseFail @CtorName "ctor" (expect $ LabelItem "constructor name")
  ]

test_symbolCtorName :: [TestTree]
test_symbolCtorName =
  [ testCase "symbol constructor name"
    $ assertParse ":!" (Symbol $ CtorName ":!")
  , testCase "symbol constructor name (single)"
    $ assertParse ":" (Symbol $ CtorName ":")
  , testCase "symbol constructor name accept :"
    $ assertParse ":+:" (Symbol $ CtorName ":+:")
  , testCase "symbol constructor name starts with :"
    $ assertParseFail @(Symbol CtorName)
        "?:"
        (expect $ LabelItem "constructor operator symbol")
  , testCase "symbol constructor name does not accept ::"
    $ assertParseFail @(Symbol CtorName) "::" mempty
  ]

test_varName :: [TestTree]
test_varName =
  [ testCase "variable name" $ assertParse "var" (VarName "var")
  , testCase "variable name accept numbers" $ assertParse "x1" (VarName "x1")
  , testCase "variable name does not accept underscore"
    $ assertParseFail @VarName "var_name" (get "_")
  , testCase "variable name does not start with uppercase"
    $ assertParseFail @VarName "Var" (expect $ LabelItem "variable name")
  ]

test_symbolVarName :: [TestTree]
test_symbolVarName =
  [ testCase "symbol variable name" $ assertParse "+" (Symbol $ VarName "+")
  , testCase "symbol variable name accept :"
    $ assertParse "!:" (Symbol $ VarName "!:")
  , testCase "symbol variable name does not start with :"
    $ assertParseFail @(Symbol VarName) ":+"
                                        (expect $ LabelItem "operator symbol")
  ]

test_tyCtorName :: [TestTree]
test_tyCtorName =
  [ testCase "type constructor name" $ assertParse "Int" (TyCtorName "Int")
  , testCase "type constructor name accept numbers"
    $ assertParse "Bool1" (TyCtorName "Bool1")
  , testCase "type constructor name does not accept underscore"
    $ assertParseFail @TyCtorName "Ty_name" (get "_")
  , testCase "type constructor name does not start with lowercase"
    $ assertParseFail @TyCtorName "tyCtor"
                                  (expect $ LabelItem "type constructor name")
  ]

test_symbolTyCtorName :: [TestTree]
test_symbolTyCtorName =
  [ testCase "symbol type constructor name"
    $ assertParse "*" (Symbol $ TyCtorName "*")
  , testCase "symbol type constructor name can start with :"
    $ assertParse ":+:" (Symbol $ TyCtorName ":+:")
  , testCase "symbol type constructor name does not accept ~"
    $ assertParseFail @(Symbol TyCtorName) "~" mempty
  ]

test_tyVarName :: [TestTree]
test_tyVarName =
  [ testCase "type variable name" $ assertParse "a" (TyVarName "a")
  , testCase "type variable name accept numbers"
    $ assertParse "b1" (TyVarName "b1")
  , testCase "type variable name does not accept underscore"
    $ assertParseFail @TyVarName "v_1" (get "_")
  , testCase "type variable name does not start with uppercase"
    $ assertParseFail @TyVarName "V" (expect $ LabelItem "type variable name")
  ]

test_className :: [TestTree]
test_className =
  [ testCase "type class name" $ assertParse "Eq" (ClassName "Eq")
  , testCase "type class name accept numbers"
    $ assertParse "Eq1" (ClassName "Eq1")
  , testCase "type class name does not accept underscore"
    $ assertParseFail @ClassName "Cls_name" (get "_")
  , testCase "type class name does not start with lowercase"
    $ assertParseFail @ClassName "num" (expect $ LabelItem "type class name")
  ]

test_moduleComponentName :: [TestTree]
test_moduleComponentName =
  [ testCase "module component name"
    $ assertParse "std" (ModuleComponentName "std")
  , testCase "module component name accept numbers"
    $ assertParse "std2" (ModuleComponentName "std2")
  , testCase "module component name accept underscore"
    $ assertParse "my_module" (ModuleComponentName "my_module")
  , testCase "module component name does not accept uppercase"
    $ assertParseFail @ModuleComponentName "myModule" (get "M")
  ]
