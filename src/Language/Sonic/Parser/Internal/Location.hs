{-# LANGUAGE NamedFieldPuns #-}

module Language.Sonic.Parser.Internal.Location
  ( withOffset
  , getOffset
  , getPosition
  , attachPosition
  , Offset
  , Position(..)
  , fromSourcePos
  )
where

import           GHC.Generics                   ( Generic )
import           GHC.Natural                    ( intToNatural )
import           Data.Data                      ( Data )
import           Numeric.Natural                ( Natural )

import           Text.Megaparsec                ( MonadParsec )
import qualified Text.Megaparsec               as Parsec
                                                ( SourcePos(..)
                                                , PosState
                                                , unPos
                                                , getOffset
                                                , getSourcePos
                                                , attachSourcePos
                                                )

import           Language.Sonic.Syntax.Location ( Located
                                                , L(..)
                                                )
import           Language.Sonic.Parser.Internal.Source
                                                ( Source )

newtype Offset = Offset { unOffset :: Int }

getOffset :: MonadParsec e s m => m Offset
getOffset = Offset <$> Parsec.getOffset

data Position
  = Position
  { line   :: Natural
  , column :: Natural
  }
  deriving (Show, Eq, Ord, Data, Generic)

fromSourcePos :: Parsec.SourcePos -> Position
fromSourcePos Parsec.SourcePos { Parsec.sourceLine, Parsec.sourceColumn } =
  position
 where
  position = Position { line, column }
  line     = intToNatural $ Parsec.unPos sourceLine
  column   = intToNatural $ Parsec.unPos sourceColumn

getPosition :: MonadParsec e s m => m Position
getPosition = fromSourcePos <$> Parsec.getSourcePos

withOffset :: MonadParsec e s m => m (a Offset) -> m (Located Offset a)
withOffset = withLocation getOffset

withLocation :: MonadParsec e s m => m l -> m (a l) -> m (Located l a)
withLocation l p = do
  begin   <- l
  content <- p
  end     <- l
  pure L { begin, content, end }

attachPosition
  :: (Source s, Traversable a)
  => Parsec.PosState s
  -> Located Offset a
  -> Located Position a
attachPosition pst a = fmap (fromSourcePos . snd) out
  where (out, _) = Parsec.attachSourcePos unOffset a pst
