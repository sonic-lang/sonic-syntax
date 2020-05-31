{-# LANGUAGE OverloadedStrings #-}

module Language.Sonic.Parser.PatternSpec
  ( test_pat
  )
where

import           TestImport

import           Language.Sonic.Syntax.Literal
import           Language.Sonic.Syntax.Name
import           Language.Sonic.Syntax.Path
import           Language.Sonic.Syntax.Pattern

test_pat :: [TestTree]
test_pat =
  [ testCase "wildcard pattern" $ assertParse "_" Wildcard
  , testCase "literal pattern" $ assertParse "32" (Literal (loc (Integer 32)))
  , testCase "variable pattern" $ assertParse "x" (Var (loc (VarName "x")))
  , testCase "tuple pattern" $ assertParse
    "(_, (x, 1))"
    (Tuple
      (loc
        (Sequence
          [ loc Wildcard
          , loc
            (Tuple
              (loc
                (Sequence
                  [ loc (Var (loc (VarName "x")))
                  , loc (Literal (loc (Integer 1)))
                  ]
                )
              )
            )
          ]
        )
      )
    )
  , testCase "constructor pattern" $ assertParse
    "mod.C _ X _"
    (Ctor
      (loc
        (Path Nothing
              (Just (loc (Sequence [loc (ModuleComponentName "mod")])))
              (loc (CtorName "C"))
        )
      )
      (Sequence
        [ loc Wildcard
        , loc (Ctor (loc (path (CtorName "X"))) (Sequence []))
        , loc Wildcard
        ]
      )
    )
  , testCase "constructor pattern with parentheses" $ assertParse
    "mod.C _ (X _)"
    (Ctor
      (loc
        (Path Nothing
              (Just (loc (Sequence [loc (ModuleComponentName "mod")])))
              (loc (CtorName "C"))
        )
      )
      (Sequence
        [ loc Wildcard
        , loc (Ctor (loc (path (CtorName "X"))) (Sequence [loc Wildcard]))
        ]
      )
    )
  , testCase "infix constructor pattern" $ assertParse
    "_ :+: _ :*: _"
    (Infix
      (loc
        (Infix (loc Wildcard)
               (loc (PatInfix (path (CtorName ":+:"))))
               (loc Wildcard)
        )
      )
      (loc (PatInfix (path (CtorName ":*:"))))
      (loc Wildcard)
    )
  , testCase "infix constructor pattern with parentheses" $ assertParse
    "_ :+: (_ :*: _)"
    (Infix
      (loc Wildcard)
      (loc (PatInfix (path (CtorName ":+:"))))
      (loc
        (Infix (loc Wildcard)
               (loc (PatInfix (path (CtorName ":*:"))))
               (loc Wildcard)
        )
      )
    )
  ]
