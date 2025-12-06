{-# LANGUAGE GADTs #-}

import           Control.Applicative
import           Data.Text           (Text)
import qualified Data.Text           as T

data ParseTree r where
  EmptyNode :: ParseTree r
  ValueNode :: r -> ParseTree r
  MapNode :: (a -> r) -> ParseTree a -> ParseTree r
  ProdNode :: (u -> v -> r) -> ParseTree u -> ParseTree v -> ParseTree r
  SumNode :: ParseTree r -> ParseTree r -> ParseTree r
  ManyNode :: ParseTree r -> ParseTree [r]

instance Functor ParseTree where
  fmap f = MapNode f

instance Applicative ParseTree where
  pure = ValueNode
  liftA2 = ProdNode

instance Alternative ParseTree where
  empty = EmptyNode
  (<|>) = SumNode
  many = ManyNode

class Resolve f where
  resolve :: f r -> Either String r

instance Resolve ParseTree where
  resolve EmptyNode         = Left "empty"
  resolve (ValueNode value) = Right value
  resolve (MapNode f p)     = fmap f $ resolve p
  resolve (ProdNode f l r)  = f <$> resolve l <*> resolve r
  resolve (SumNode l r)     = resolve l <> resolve r
  resolve (ManyNode p)      = Right []
