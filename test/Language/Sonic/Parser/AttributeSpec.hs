{-# LANGUAGE OverloadedStrings #-}

module Language.Sonic.Parser.AttributeSpec
  ( test_attrSet
  , test_attr
  , test_attrValue
  , test_attrValueList
  )
where

import           TestImport

import           Language.Sonic.Syntax.Name
import           Language.Sonic.Syntax.Path
import           Language.Sonic.Syntax.Attribute

test_attrValue :: [TestTree]
test_attrValue =
  [ testCase "string" $ assertParse "\"windows\"" (TextValue "windows")
  , testCase "path (variable)" $ assertParse
    "$std.print"
    (PathValue
      (Path (Just (loc Dollar))
            (Just (loc (Sequence [loc (ModuleComponentName "std")])))
            (loc (ValueEntityName (loc (VarValueName (VarName "print")))))
      )
    )
  , testCase "path (type)" $ assertParse
    "#prim.'Int"
    (PathValue
      (Path (Just (loc Hash))
            (Just (loc (Sequence [loc (ModuleComponentName "prim")])))
            (loc (TypeEntityName (loc (CtorTypeName (TyCtorName "Int")))))
      )
    )
  ]

test_attrValueList :: [TestTree]
test_attrValueList =
  [ testCase "simple paths" $ assertParse
    "(''Show, ''Eq)"
    (AttrValueList
      (Sequence
        [ loc (PathValue (path (ClassEntityName (loc (ClassName "Show")))))
        , loc (PathValue (path (ClassEntityName (loc (ClassName "Eq")))))
        ]
      )
    )
  , testCase "strings" $ assertParse
    "(\"hello\", \"world\")"
    (AttrValueList (Sequence [loc (TextValue "hello"), loc (TextValue "world")])
    )
  , testCase "heterogeous" $ assertParse
    "(\"hello\", world)"
    (AttrValueList
      (Sequence
        [ loc (TextValue "hello")
        , loc
          (PathValue
            (path (ValueEntityName (loc (VarValueName (VarName "world")))))
          )
        ]
      )
    )
  ]

test_attr :: [TestTree]
test_attr =
  [ testCase "name attr" $ assertParse "attr" (Name (loc (AttrKeyName "attr")))
  , testCase "value attr" $ assertParse
    "cfg = \"windows\""
    (Value (loc (AttrKeyName "cfg")) (loc (TextValue "windows")))
  , testCase "list attr" $ assertParse
    "derive(show, eq)"
    (List
      (loc (AttrKeyName "derive"))
      (loc
        (AttrValueList
          (Sequence
            [ loc
              (PathValue
                (path (ValueEntityName (loc (VarValueName (VarName "show")))))
              )
            , loc
              (PathValue
                (path (ValueEntityName (loc (VarValueName (VarName "eq")))))
              )
            ]
          )
        )
      )
    )
  , testCase "record attr" $ assertParse
    "build[debug, os = \"linux\"]"
    (Record
      (loc (AttrKeyName "build"))
      (loc
        (AttrSet
          (Sequence
            [ loc (Name (loc (AttrKeyName "debug")))
            , loc (Value (loc (AttrKeyName "os")) (loc (TextValue "linux")))
            ]
          )
        )
      )
    )
  ]

test_attrSet :: [TestTree]
test_attrSet =
  [ testCase "simple" $ assertParse
    "[attr]"
    (AttrSet (Sequence [loc (Name (loc (AttrKeyName "attr")))]))
  , testCase "nested" $ assertParse
    "[build[debug]]"
    (AttrSet
      (Sequence
        [ loc
            (Record
              (loc (AttrKeyName "build"))
              (loc (AttrSet (Sequence [loc (Name (loc (AttrKeyName "debug")))]))
              )
            )
        ]
      )
    )
  , testCase "multiple" $ assertParse
    "[attr1, attr2]"
    (AttrSet
      (Sequence
        [ loc (Name (loc (AttrKeyName "attr1")))
        , loc (Name (loc (AttrKeyName "attr2")))
        ]
      )
    )
  ]
