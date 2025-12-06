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
