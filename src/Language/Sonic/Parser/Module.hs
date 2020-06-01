module Language.Sonic.Parser.Module
  ( moduleParser
  , moduleItemParser
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Control.Applicative.Combinators
                                                ( optional )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source )
import           Language.Sonic.Parser.Internal.Location
                                                ( Offset
                                                , withOffset
                                                )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse )
import           Language.Sonic.Parser.Internal.Lexer
                                                ( symbol )
import           Language.Sonic.Parser.Attribute
                                                ( withAttrSetParser
                                                , attrSetParser
                                                )
import           Language.Sonic.Parser.Declaration
                                                ( declParser )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Module   ( Module(..)
                                                , ModuleItem(..)
                                                )

moduleParser :: Source s => Parse s (Module Offset)
moduleParser = do
  decls <- many (withOffset moduleItemParser <* optional (symbol ";;"))
  pure . Module $ Sequence decls

moduleItemParser :: Source s => Parse s (ModuleItem Offset)
moduleItemParser = TopAttr <$> attr <|> TopDecl <$> decl
 where
  attr = withOffset (symbol "#!" *> attrSetParser)
  decl = withOffset (withAttrSetParser declParser)
