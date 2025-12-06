{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Applicative
import           Control.Monad.Trans.State
import qualified Data.List                 as List
import           Data.Text                 (Text)
import qualified Data.Text                 as T

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

--------------------------------------------------------------------------------

data Token
  = LongOption Text
  | Argument Text
  deriving (Show)

class Accepts a where
  accepts :: a -> Token -> Bool

instance Accepts (ParseTree a) where
  accepts (MapNode _ p) token    = accepts p token
  accepts (ProdNode _ l r) token = accepts l token || accepts r token
  accepts (SumNode l r) token    = accepts l token || accepts r token
  accepts (ManyNode p) token     = accepts p token
  accepts _ _                    = False

--------------------------------------------------------------------------------

type Stream = State [Text]

runStream :: Stream a -> [Text] -> (a, [Text])
runStream = runState

pop :: Stream (Maybe Text)
pop = state $ \case
  [] -> (Nothing, [])
  (x:xs) -> (Just x, xs)

peek :: Stream (Maybe Text)
peek = gets $ fmap fst . List.uncons
