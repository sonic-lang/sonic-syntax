{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Language.Sonic.Parser.TypeSpec
  ( test_type
  , test_tyVarBinder
  , test_context
  , test_predicate
  )
where

import           TestImport

import           Language.Sonic.Syntax.Name
import           Language.Sonic.Syntax.Kind
import           Language.Sonic.Syntax.Type

test_type :: [TestTree]
test_type =
  [ testCase "type variable" $ assertParse "a" (Var (loc (TyVarName "a")))
  , testCase "type constructor"
    $ assertParse "Int" (Ctor (loc (path (TyCtorName "Int"))))
  , testCase "tuple type" $ assertParse
    "(Int, a)"
    (Tuple
      (loc
        (Sequence
          [ loc (Ctor (loc (path (TyCtorName "Int"))))
          , loc (Var (loc (TyVarName "a")))
          ]
        )
      )
    )
  , testCase "applied type" $ assertParse
    "Either a b"
    (Apply
      (loc
        (Apply (loc (Ctor (loc (path (TyCtorName "Either")))))
               (loc (Var (loc (TyVarName "a"))))
        )
      )
      (loc (Var (loc (TyVarName "b"))))
    )
  , testCase "applied type with parentheses" $ assertParse
    "Maybe (f a)"
    (Apply
      (loc (Ctor (loc (path (TyCtorName "Maybe")))))
      (loc
        (Apply (loc (Var (loc (TyVarName "f"))))
               (loc (Var (loc (TyVarName "a"))))
        )
      )
    )
  , testCase "infix type operator application" $ assertParse
    "a -> b -> c"
    (InfixApply
      (loc
        (InfixApply (loc (Var (loc (TyVarName "a"))))
                    (loc (TypeInfix (path (TyCtorName "->"))))
                    (loc (Var (loc (TyVarName "b"))))
        )
      )
      (loc (TypeInfix (path (TyCtorName "->"))))
      (loc (Var (loc (TyVarName "c"))))
    )
  , testCase "kind annotation" $ assertParse
    "a :: Type -> Type"
    (Annotate (loc (Var (loc (TyVarName "a"))))
              (loc (Arrow (loc Type) (loc Type)))
    )
  , testCase "universal quantification" $ assertParse
    "forall f a. Functor f => f a"
    (Forall
      (Sequence
        [ loc (TyVarBinder (loc (TyVarName "f")) Nothing)
        , loc (TyVarBinder (loc (TyVarName "a")) Nothing)
        ]
      )
      (Just
        (loc
          (Context
            (loc
              (Sequence
                [ loc
                    (Class (loc (path (ClassName "Functor")))
                           (Sequence [loc (Var (loc (TyVarName "f")))])
                    )
                ]
              )
            )
          )
        )
      )
      (loc
        (Apply (loc (Var (loc (TyVarName "f"))))
               (loc (Var (loc (TyVarName "a"))))
        )
      )
    )
  , testCase ":: cannot be used as infix type operator"
    $ assertParseFail @Type "a :: (a, b)" mempty
  , testCase "~ cannot be used as infix type operator"
    $ assertParseFail @Type "a ~ b" (get "~")
  ]

test_tyVarBinder :: [TestTree]
test_tyVarBinder =
  [ testCase "type variable binder"
    $ assertParse "a" (TyVarBinder (loc (TyVarName "a")) Nothing)
  , testCase "type variable binder with kind" $ assertParse
    "(a :: Type -> Type)"
    (TyVarBinder (loc (TyVarName "a"))
                 (Just (loc (Arrow (loc Type) (loc Type))))
    )
  ]

test_context :: [TestTree]
test_context =
  [ testCase "type context with single predicates" $ assertParse
    "Eq a"
    (Context
      (loc
        (Sequence
          [ loc
              (Class (loc (path (ClassName "Eq")))
                     (Sequence [loc (Var (loc (TyVarName "a")))])
              )
          ]
        )
      )
    )
  , testCase "type context with multiple predicates" $ assertParse
    "(Eq a, Show a)"
    (Context
      (loc
        (Sequence
          [ loc
            (Class (loc (path (ClassName "Eq")))
                   (Sequence [loc (Var (loc (TyVarName "a")))])
            )
          , loc
            (Class (loc (path (ClassName "Show")))
                   (Sequence [loc (Var (loc (TyVarName "a")))])
            )
          ]
        )
      )
    )
  , testCase "type context contains tuples" $ assertParse
    "(Eq (a, b), Show a)"
    (Context
      (loc
        (Sequence
          [ loc
            (Class
              (loc (path (ClassName "Eq")))
              (Sequence
                [ loc
                    (Tuple
                      (loc
                        (Sequence
                          [ loc (Var (loc (TyVarName "a")))
                          , loc (Var (loc (TyVarName "b")))
                          ]
                        )
                      )
                    )
                ]
              )
            )
          , loc
            (Class (loc (path (ClassName "Show")))
                   (Sequence [loc (Var (loc (TyVarName "a")))])
            )
          ]
        )
      )
    )
  ]

test_predicate :: [TestTree]
test_predicate =
  [ testCase "class predicate" $ assertParse
    "C a b"
    (Class
      (loc (path (ClassName "C")))
      (Sequence
        [loc (Var (loc (TyVarName "a"))), loc (Var (loc (TyVarName "b")))]
      )
    )
  , testCase "nullary class predicate"
    $ assertParse "C" (Class (loc (path (ClassName "C"))) (Sequence []))
  , testCase "equality predicate" $ assertParse
    "Int ~ a"
    (Equality (loc (Ctor (loc (path (TyCtorName "Int")))))
              (loc (Var (loc (TyVarName "a"))))
    )
  , testCase "equality predicate with infix operators" $ assertParse
    "Int -> b ~ a"
    (Equality
      (loc
        (InfixApply (loc (Ctor (loc (path (TyCtorName "Int")))))
                    (loc (TypeInfix (path (TyCtorName "->"))))
                    (loc (Var (loc (TyVarName "b"))))
        )
      )
      (loc (Var (loc (TyVarName "a"))))
    )
  ]
