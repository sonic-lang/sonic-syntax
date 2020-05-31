{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Language.Sonic.Parser.Name
  ( ctorNameParser
  , symbolCtorNameParser
  , varNameParser
  , symbolVarNameParser
  , tyCtorNameParser
  , symbolTyCtorNameParser
  , tyVarNameParser
  , classNameParser
  , moduleComponentNameParser
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Coerce                    ( Coercible
                                                , coerce
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
                                                ( cons )
import           Data.Char                      ( isUpper
                                                , isLower
                                                , isAlphaNum
                                                , isNumber
                                                )
import           Control.Monad                  ( when )

import           Text.Megaparsec                ( takeWhileP
                                                , satisfy
                                                , try
                                                , (<?>)
                                                )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source
                                                , fromChunk
                                                , toChunk
                                                )
import           Language.Sonic.Parser.Internal.Location
                                                ( Offset )
import           Language.Sonic.Parser.Internal.Error
                                                ( TokenItem(..) )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse
                                                , unexpectedChunk
                                                )
import           Language.Sonic.Parser.Internal.Lexer
                                                ( lexeme )
import           Language.Sonic.Syntax.Name     ( CtorName(..)
                                                , VarName(..)
                                                , TyCtorName(..)
                                                , TyVarName(..)
                                                , ClassName(..)
                                                , ModuleComponentName(..)
                                                )

reservedWords :: [Text]
reservedWords = map
  pack
  [ "let"
  , "in"
  , "case"
  , "if"
  , "data"
  , "instance"
  , "class"
  , "where"
  , "Type"
  , "forall"
  , "="
  , "=>"
  , "::"
  , "~"
  ]

isOperatorChar :: Char -> Bool
isOperatorChar = (`elem` chars)
 where
  chars =
    [ '!'
    , '%'
    , '&'
    , '*'
    , '+'
    , '/'
    , '<'
    , '+'
    , '>'
    , '?'
    , '\\'
    , '^'
    , '-'
    , '~'
    , '='
    , ':'
    , '@'
    ]

class Name a where
  description :: Proxy a -> String
  initialLetter :: Proxy a -> Char -> Bool
  followingLetter :: Proxy a -> Char -> Bool
  fromText :: Text -> a l

  default fromText :: Coercible Text (a l) => Text -> (a l)
  fromText = coerce

newtype Symbol a l = Symbol (a l)

symbolParser :: Source s => Parse s (Symbol a Offset) -> Parse s (a Offset)
symbolParser p = do
  Symbol s <- p
  pure s

instance Name CtorName where
  description _ = "constructor name"
  initialLetter _ = isUpper
  followingLetter _ = isAlphaNum

instance Name VarName where
  description _ = "variable name"
  initialLetter _ = isLower
  followingLetter _ = isAlphaNum

instance Name TyCtorName where
  description _ = "type constructor name"
  initialLetter _ = isUpper
  followingLetter _ = isAlphaNum

instance Name TyVarName where
  description _ = "type variable name"
  initialLetter _ = isLower
  followingLetter _ = isAlphaNum

instance Name ClassName where
  description _ = "type class name"
  initialLetter _ = isUpper
  followingLetter _ = isAlphaNum

instance Name ModuleComponentName where
  description _ = "module component name"
  initialLetter _ = isLower
  followingLetter _ c = isNumber c || isLower c || c == '_'

instance Name (Symbol CtorName) where
  description _ = "constructor operator symbol"
  initialLetter _ = (== ':')
  followingLetter _ = isOperatorChar

instance Name (Symbol VarName) where
  description _ = "operator symbol"
  initialLetter _ c = isOperatorChar c && c /= ':'
  followingLetter _ = isOperatorChar

instance Name (Symbol TyCtorName) where
  description _ = "type operator symbol"
  initialLetter _ = isOperatorChar
  followingLetter _ = isOperatorChar

check :: Source s => Parse s Text -> Parse s Text
check p = do
  t <- p
  when (t `elem` reservedWords)
    $ unexpectedChunk (toChunk t) (LabelItem "identifier")
  pure t

nameParser :: forall a s . (Source s, Name a) => Parse s (a Offset)
nameParser = fromText <$> lexeme (try name) <?> description (Proxy @a)
 where
  name = check $ do
    c    <- satisfy $ initialLetter (Proxy @a)
    rest <- takeWhileP Nothing $ followingLetter (Proxy @a)
    pure . Text.cons c $ fromChunk rest

ctorNameParser :: Source s => Parse s (CtorName Offset)
ctorNameParser = nameParser

symbolCtorNameParser :: Source s => Parse s (CtorName Offset)
symbolCtorNameParser = symbolParser nameParser

varNameParser :: Source s => Parse s (VarName Offset)
varNameParser = nameParser

symbolVarNameParser :: Source s => Parse s (VarName Offset)
symbolVarNameParser = symbolParser nameParser

tyCtorNameParser :: Source s => Parse s (TyCtorName Offset)
tyCtorNameParser = nameParser

symbolTyCtorNameParser :: Source s => Parse s (TyCtorName Offset)
symbolTyCtorNameParser = symbolParser nameParser

tyVarNameParser :: Source s => Parse s (TyVarName Offset)
tyVarNameParser = nameParser

classNameParser :: Source s => Parse s (ClassName Offset)
classNameParser = nameParser

moduleComponentNameParser :: Source s => Parse s (ModuleComponentName Offset)
moduleComponentNameParser = nameParser
