{-# LANGUAGE NamedFieldPuns #-}

module Language.Sonic.Parser.Path
  ( pathPrefixParser
  , modulePathParser
  , pathParser
  )
where

import           Control.Applicative            ( some )
import           Control.Applicative.Combinators
                                                ( optional )

import           Text.Megaparsec                ( try )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source )
import           Language.Sonic.Parser.Internal.Location
                                                ( Offset
                                                , withOffset
                                                )
import           Language.Sonic.Parser.Internal.Error
                                                ( chunkItem )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse
                                                , matchToken
                                                )
import           Language.Sonic.Parser.Internal.Lexer
                                                ( symbol )
import           Language.Sonic.Parser.Name     ( moduleComponentNameParser )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Name     ( ModuleComponentName(..) )
import           Language.Sonic.Syntax.Path     ( PathPrefix(..)
                                                , Path(..)
                                                )

pathPrefixParser :: Source s => Parse s (PathPrefix Offset)
pathPrefixParser = matchToken match expected
 where
  expected = [chunkItem ".", chunkItem ":", chunkItem "#"]
  match '.' = Just Dot
  match ':' = Just Colon
  match '#' = Just Hash
  match _   = Nothing

modulePathParser :: Source s => Parse s (Sequence ModuleComponentName Offset)
modulePathParser =
  Sequence <$> some (try $ withOffset moduleComponentNameParser <* symbol ":")

pathParser :: Source s => Parse s (n Offset) -> Parse s (Path n Offset)
pathParser n = do
  prefix     <- optional (withOffset pathPrefixParser)
  modulePath <- optional $ withOffset modulePathParser
  name       <- withOffset n
  pure Path { prefix, modulePath, name }
