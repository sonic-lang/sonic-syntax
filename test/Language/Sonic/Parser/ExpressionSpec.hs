{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Language.Sonic.Parser.ExpressionSpec
  ( test_expr
  , test_exprInfix
  , test_letDefn
  , test_letBinder
  , test_caseArm
  , test_guard
  )
where

import           TestImport

import           Language.Sonic.Syntax.Literal
import           Language.Sonic.Syntax.Name
import           Language.Sonic.Syntax.Path
import qualified Language.Sonic.Syntax.Pattern as P
import qualified Language.Sonic.Syntax.Type    as T
import           Language.Sonic.Syntax.Expression

test_expr :: [TestTree]
test_expr =
  [ testCase "variable" $ assertParse "a" (Var (loc (path (VarName "a"))))
  , testCase "constructor"
    $ assertParse "None" (Ctor (loc (path (CtorName "None"))))
  , testCase "literal" $ assertParse "43" (Literal (loc (Integer 43)))
  , testCase "tuple" $ assertParse
    "(None, a)"
    (Tuple
      (loc
        (Sequence
          [ loc (Ctor (loc (path (CtorName "None"))))
          , loc (Var (loc (path (VarName "a"))))
          ]
        )
      )
    )
  , testCase "application" $ assertParse
    "Just 1"
    (Apply (loc (Ctor (loc (path (CtorName "Just")))))
           (loc (Literal (loc (Integer 1))))
    )
  , testCase "application with parentheses 1" $ assertParse
    "Just (f 1)"
    (Apply
      (loc (Ctor (loc (path (CtorName "Just")))))
      (loc
        (Apply (loc (Var (loc (path (VarName "f")))))
               (loc (Literal (loc (Integer 1))))
        )
      )
    )
  , testCase "application with parentheses 2" $ assertParse
    "Both 1 2"
    (Apply
      (loc
        (Apply (loc (Ctor (loc (path (CtorName "Both")))))
               (loc (Literal (loc (Integer 1))))
        )
      )
      (loc (Literal (loc (Integer 2))))
    )
  , testCase "infix operator" $ assertParse
    "1 + 2 * 3"
    (InfixApply
      (loc
        (InfixApply (loc (Literal (loc (Integer 1))))
                    (loc (VarInfix (path (VarName "+"))))
                    (loc (Literal (loc (Integer 2))))
        )
      )
      (loc (VarInfix (path (VarName "*"))))
      (loc (Literal (loc (Integer 3))))
    )
  , testCase "infix operator with parentheses" $ assertParse
    "1 + (2 * 3)"
    (InfixApply
      (loc (Literal (loc (Integer 1))))
      (loc (VarInfix (path (VarName "+"))))
      (loc
        (InfixApply (loc (Literal (loc (Integer 2))))
                    (loc (VarInfix (path (VarName "*"))))
                    (loc (Literal (loc (Integer 3))))
        )
      )
    )
  , testCase "lambda" $ assertParse
    "\\x. x"
    (Lambda (Sequence [loc (P.Var (loc (VarName "x")))])
            (loc (Var (loc (path (VarName "x")))))
    )
  , testCase "lambda with multiple patterns 1" $ assertParse
    "\\X x _. x"
    (Lambda
      (Sequence
        [ loc (P.Ctor (loc (path (CtorName "X"))) (Sequence []))
        , loc (P.Var (loc (VarName "x")))
        , loc P.Wildcard
        ]
      )
      (loc (Var (loc (path (VarName "x")))))
    )
  , testCase "lambda with multiple patterns 2" $ assertParse
    "\\(X x) _. x"
    (Lambda
      (Sequence
        [ loc
          (P.Ctor (loc (path (CtorName "X")))
                  (Sequence [loc (P.Var (loc (VarName "x")))])
          )
        , loc P.Wildcard
        ]
      )
      (loc (Var (loc (path (VarName "x")))))
    )
  , testCase "let" $ assertParse
    "let x = 1 in z"
    (Let
      (loc
        (Sequence
          [ loc
              (LetDefn (loc (PatBinder (loc (P.Var (loc (VarName "x"))))))
                       (loc (Literal (loc (Integer 1))))
              )
          ]
        )
      )
      (loc (Var (loc (path (VarName "z")))))
    )
  , testCase "let with multiple definitons" $ assertParse
    "let { x = y, y = 1 } in z"
    (Let
      (loc
        (Sequence
          [ loc
            (LetDefn (loc (PatBinder (loc (P.Var (loc (VarName "x"))))))
                     (loc (Var (loc (path (VarName "y")))))
            )
          , loc
            (LetDefn (loc (PatBinder (loc (P.Var (loc (VarName "y"))))))
                     (loc (Literal (loc (Integer 1))))
            )
          ]
        )
      )
      (loc (Var (loc (path (VarName "z")))))
    )
  , testCase "case" $ assertParse
    "case x { C x if x == 0 => x, C x => x }"
    (Case
      (loc (Var (loc (path (VarName "x")))))
      (loc
        (Sequence
          [ loc
            (CaseArm
              (loc
                (P.Ctor (loc (path (CtorName "C")))
                        (Sequence [loc (P.Var (loc (VarName "x")))])
                )
              )
              (Just
                (loc
                  (Guard
                    (loc
                      (InfixApply (loc (Var (loc (path (VarName "x")))))
                                  (loc (VarInfix (path (VarName "=="))))
                                  (loc (Literal (loc (Integer 0))))
                      )
                    )
                  )
                )
              )
              (loc (Var (loc (path (VarName "x")))))
            )
          , loc
            (CaseArm
              (loc
                (P.Ctor (loc (path (CtorName "C")))
                        (Sequence [loc (P.Var (loc (VarName "x")))])
                )
              )
              Nothing
              (loc (Var (loc (path (VarName "x")))))
            )
          ]
        )
      )
    )
  , testCase "infix operator starts with \\" $ assertParse
    "True \\/ False"
    (InfixApply (loc (Ctor (loc (path (CtorName "True")))))
                (loc (VarInfix (path (VarName "\\/"))))
                (loc (Ctor (loc (path (CtorName "False")))))
    )
  ]

test_exprInfix :: [TestTree]
test_exprInfix =
  [ testCase "simple infix operator 1"
    $ assertParse "+" (VarInfix (path (VarName "+")))
  , testCase "simple infix operator 2"
    $ assertParse "\\/" (VarInfix (path (VarName "\\/")))
  , testCase "simple constructor infix operator"
    $ assertParse ":!" (CtorInfix (path (CtorName ":!")))
  , testCase "quoted infix operator"
    $ assertParse "`elem`" (VarInfix (path (VarName "elem")))
  , testCase "quoted constructor infix operator"
    $ assertParse "`Cons`" (CtorInfix (path (CtorName "Cons")))
  , testCase "quoted infix operator does not accept expression"
    $ assertParseFail @ExprInfix "`a b`" mempty
  , testCase "quoted infix operator accept path" $ assertParse
    "`a.b`"
    (VarInfix
      (Path Nothing
            (Just (loc (Sequence [loc (ModuleComponentName "a")])))
            (loc (VarName "b"))
      )
    )
  ]

test_letDefn :: [TestTree]
test_letDefn =
  [ testCase "simple let definition" $ assertParse
    "x = 1"
    (LetDefn (loc (PatBinder (loc (P.Var (loc (VarName "x"))))))
             (loc (Literal (loc (Integer 1))))
    )
  , testCase "let definition with pattern" $ assertParse
    "(x, y) = z"
    (LetDefn
      (loc
        (PatBinder
          (loc
            (P.Tuple
              (loc
                (Sequence
                  [ loc (P.Var (loc (VarName "x")))
                  , loc (P.Var (loc (VarName "y")))
                  ]
                )
              )
            )
          )
        )
      )
      (loc (Var (loc (path (VarName "z")))))
    )
  , testCase "let definition with type annotation" $ assertParse
    "x :: Int = 1"
    (LetDefn
      (loc
        (AnnotatedBinder (loc (VarName "x"))
                         (loc (T.Ctor (loc (path (TyCtorName "Int")))))
        )
      )
      (loc (Literal (loc (Integer 1))))
    )
  ]

test_letBinder :: [TestTree]
test_letBinder =
  [ testCase "simple let binder"
    $ assertParse "x" (PatBinder (loc (P.Var (loc (VarName "x")))))
  , testCase "pattern let binder" $ assertParse
    "C _ x"
    (PatBinder
      (loc
        (P.Ctor (loc (path (CtorName "C")))
                (Sequence [loc P.Wildcard, loc (P.Var (loc (VarName "x")))])
        )
      )
    )
  , testCase "let binder with type annotation" $ assertParse
    "x :: Int"
    (AnnotatedBinder (loc (VarName "x"))
                     (loc (T.Ctor (loc (path (TyCtorName "Int")))))
    )
  , testCase "type annotation is not allowed in pattern binder"
    $ assertParseFail @LetBinder "(x, _) :: Int" (get ":")
  ]

test_caseArm :: [TestTree]
test_caseArm =
  [ testCase "simple case arm" $ assertParse
    "C x => x"
    (CaseArm
      (loc
        (P.Ctor (loc (path (CtorName "C")))
                (Sequence [loc (P.Var (loc (VarName "x")))])
        )
      )
      Nothing
      (loc (Var (loc (path (VarName "x")))))
    )
  , testCase "case arm with guard" $ assertParse
    "C x if x == 0 => x"
    (CaseArm
      (loc
        (P.Ctor (loc (path (CtorName "C")))
                (Sequence [loc (P.Var (loc (VarName "x")))])
        )
      )
      (Just
        (loc
          (Guard
            (loc
              (InfixApply (loc (Var (loc (path (VarName "x")))))
                          (loc (VarInfix (path (VarName "=="))))
                          (loc (Literal (loc (Integer 0))))
              )
            )
          )
        )
      )
      (loc (Var (loc (path (VarName "x")))))
    )
  ]

test_guard :: [TestTree]
test_guard =
  [ testCase "simple guard" $ assertParse
      "if x > 0"
      (Guard
        (loc
          (InfixApply (loc (Var (loc (path (VarName "x")))))
                      (loc (VarInfix (path (VarName ">"))))
                      (loc (Literal (loc (Integer 0))))
          )
        )
      )
  ]
