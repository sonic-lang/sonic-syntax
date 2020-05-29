module Language.Sonic.Parser.Name
  ( ctorNameParser
  , varNameParser
  , tyCtorNameParser
  , tyVarNameParser
  , classNameParser
  , moduleComponentNameParser
  )
where

import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
                                                ( cons )
import           Data.Char                      ( isUpper
                                                , isLower
                                                , isAlphaNum
                                                )
import           Control.Monad                  ( when )

import           Text.Megaparsec                ( takeWhileP
                                                , satisfy
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

camelCaseReservedWords :: [Text]
camelCaseReservedWords =
  map pack ["let", "case", "if", "data", "instance", "class", "where"]

check :: Source s => [Text] -> Parse s Text -> Parse s Text
check rws p = do
  t <- p
  when (t `elem` rws) $ unexpectedChunk (toChunk t) (Label $ pack "identifier")
  pure t

pascalCase :: Source s => Parse s Text
pascalCase = do
  c    <- satisfy isUpper <?> "upper char"
  rest <- takeWhileP (Just "name") isAlphaNum
  pure $ Text.cons c (fromChunk rest)

camelCase :: Source s => Parse s Text
camelCase = check camelCaseReservedWords $ do
  c    <- satisfy isLower <?> "lower char"
  rest <- takeWhileP (Just "name") isAlphaNum
  pure $ Text.cons c (fromChunk rest)

snakeCase :: Source s => Parse s Text
snakeCase = do
  c    <- satisfy isLower <?> "lower char"
  rest <- takeWhileP (Just "name") (isAlphaNum |.| isLower |.| (== '_'))
  pure $ Text.cons c (fromChunk rest)
  where a |.| b = \c -> a c || b c


ctorNameParser :: Source s => Parse s (CtorName Offset)
ctorNameParser = CtorName <$> lexeme pascalCase <?> "constructor name"

varNameParser :: Source s => Parse s (VarName Offset)
varNameParser = VarName <$> lexeme camelCase <?> "variable name"

tyCtorNameParser :: Source s => Parse s (TyCtorName Offset)
tyCtorNameParser = TyCtorName <$> lexeme pascalCase <?> "type constructor name"

tyVarNameParser :: Source s => Parse s (TyVarName Offset)
tyVarNameParser = TyVarName <$> lexeme camelCase <?> "type variable name"

classNameParser :: Source s => Parse s (ClassName Offset)
classNameParser = ClassName <$> lexeme pascalCase <?> "type class name"

moduleComponentNameParser :: Source s => Parse s (ModuleComponentName Offset)
moduleComponentNameParser =
  ModuleComponentName <$> lexeme snakeCase <?> "module component name"
