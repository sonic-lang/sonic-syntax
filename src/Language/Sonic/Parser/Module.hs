module Language.Sonic.Parser.Module
  ( moduleParser
  )
where

import           Control.Applicative            ( Alternative(..) )

import           Language.Sonic.Parser.Internal.Source
                                                ( Source )
import           Language.Sonic.Parser.Internal.Location
                                                ( Offset
                                                , withOffset
                                                )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse )
import           Language.Sonic.Parser.Declaration
                                                ( declParser )
import           Language.Sonic.Syntax.Sequence ( Sequence(..) )
import           Language.Sonic.Syntax.Module   ( Module(..) )

moduleParser :: Source s => Parse s (Module Offset)
moduleParser = do
  decls <- many (withOffset declParser)
  pure . Module $ Sequence decls
