{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Sonic.Parser.Internal.Operator
  ( withOperators
  -- * Precedence Table
  , BinaryOp
  , UnaryOp
  , Operators
  -- ** Constructors
  , infixNoneOp
  , infixLeftOp
  , infixRightOp
  , prefixOp
  , postfixOp
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Control.Applicative.Combinators
                                                ( option
                                                , choice
                                                )

import           Lens.Micro
import           Lens.Micro.TH                  ( makeLenses )

import           Language.Sonic.Parser.Internal.Location
                                                ( Offset
                                                , withOffset
                                                )
import           Language.Sonic.Parser.Internal.Source
                                                ( Source )
import           Language.Sonic.Parser.Internal.Parse
                                                ( Parse )
import           Language.Sonic.Syntax.Location ( Located
                                                , noLoc
                                                )

-- | @'BinaryOp' s a@ parses a corresponding binary operator and produces a constructor of the expression
type BinaryOp s a = Parse s (Located Offset a -> Located Offset a -> a Offset)

-- | @'UnaryOp' s a@ parses a corresponding unary operator and produces a constructor of the expression
type UnaryOp s a = Parse s (Located Offset a -> a Offset)

-- | List of operators at a level of precedence
data Operators s a
  = Operators
  { _infixNone  :: [BinaryOp s a]
  , _infixLeft  :: [BinaryOp s a]
  , _infixRight :: [BinaryOp s a]
  , _prefix     :: [UnaryOp s a]
  , _postfix    :: [UnaryOp s a]
  }

$(makeLenses ''Operators)

initOperators :: Operators s a
initOperators = Operators [] [] [] [] []

-- | Reify a 'Lens' for 'Operators' so it can be stored safely in a container
data ReifiedOpLens s a = forall op. OpLens (Lens' (Operators s a) [op])

instance Semigroup (Operators s a) where
  a <> b = foldr add initOperators rows
   where
    add (OpLens g) ops = ops & g .~ (a, b) ^. both . g
    rows =
      [ OpLens infixNone
      , OpLens infixLeft
      , OpLens infixRight
      , OpLens prefix
      , OpLens postfix
      ]

instance Monoid (Operators s a) where
  mempty = initOperators

singleton :: ASetter' (Operators s a) [op] -> op -> Operators s a
singleton l v = initOperators & l .~ [v]

infixNoneOp, infixLeftOp, infixRightOp :: BinaryOp s a -> Operators s a
infixNoneOp = singleton infixNone
infixLeftOp = singleton infixLeft
infixRightOp = singleton infixRight

prefixOp, postfixOp :: UnaryOp s a -> Operators s a
prefixOp = singleton prefix
postfixOp = singleton postfix

-- | @'withOperators' table atom@ builds an expression parser with precedence table @table@.
-- The table contains operators in descending order, i.e. the initial row has the highest priority.
withOperators
  :: Source s => [Operators s a] -> Parse s (a Offset) -> Parse s (a Offset)
withOperators table atom = foldl addPrecLevel atom table

addPrecLevel
  :: forall s a
   . Source s
  => Parse s (a Offset)
  -> Operators s a
  -> Parse s (a Offset)
addPrecLevel atom table = do
  lhs <- withOffset term
  choice [right lhs, left lhs, none lhs, pure (noLoc lhs)]
 where
  term = post
  pre  = do
    op <- option noLoc $ choiceOp prefix
    op <$> withOffset atom
  post = do
    x  <- withOffset pre
    op <- option noLoc $ choiceOp postfix
    pure $ op x
  right lhs = do
    op  <- choiceOp infixRight
    rhs <- withOffset $ do
      x <- withOffset term
      right x <|> pure (noLoc x)
    pure $ op lhs rhs
  left lhs = do
    x <- withOffset $ do
      op  <- choiceOp infixLeft
      rhs <- withOffset term
      pure $ op lhs rhs
    left x <|> pure (noLoc x)
  none lhs = do
    op  <- choiceOp infixNone
    rhs <- withOffset term
    pure $ op lhs rhs
  choiceOp :: Lens' (Operators s a) [Parse s op] -> Parse s op
  choiceOp l = table ^. l . to choice
