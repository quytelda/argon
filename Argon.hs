{-# LANGUAGE GADTs        #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import qualified Data.List                 as List
import           Data.Text                 (Text)
import qualified Data.Text                 as T

-- | 'ParseTree p r' is an expression tree built from parsers of type
-- 'p' which evaluates to a value of type 'r' supplied with the proper
-- input.
data ParseTree p r where
  EmptyNode :: ParseTree p r -- ^ Terminal node with no value
  ValueNode :: r -> ParseTree p r -- ^ Terminal node with a resolved value
  ParseNode :: p r -> ParseTree p r -- ^ A parser awaiting input
  MapNode :: (a -> r) -> ParseTree p a -> ParseTree p r -- ^ Abstracts fmap
  ProdNode :: (u -> v -> r) -> ParseTree p u -> ParseTree p v -> ParseTree p r -- ^ Abstracts liftA2
  SumNode :: ParseTree p r -> ParseTree p r -> ParseTree p r -- ^ Abstracts (<|>)
  ManyNode :: ParseTree p r -> ParseTree p [r] -- ^ Abstracts many

instance Functor (ParseTree p) where
  fmap f = MapNode f

instance Applicative (ParseTree p) where
  pure = ValueNode
  liftA2 = ProdNode

instance Alternative (ParseTree p) where
  empty = EmptyNode
  (<|>) = SumNode
  many = ManyNode

class Resolve f where
  resolve :: f r -> Either String r

instance Resolve p => Resolve (ParseTree p) where
  resolve EmptyNode          = Left "empty"
  resolve (ValueNode value)  = Right value
  resolve (ParseNode parser) = resolve parser
  resolve (MapNode f p)      = fmap f $ resolve p
  resolve (ProdNode f l r)   = f <$> resolve l <*> resolve r
  resolve (SumNode l r)      = resolve l <> resolve r
  resolve (ManyNode p)       = Right []

--------------------------------------------------------------------------------
-- Stream Monad

type Stream = State [Text]

runStream :: Stream a -> [Text] -> (a, [Text])
runStream = runState

isEmptyStream :: Stream Bool
isEmptyStream = gets null

pop :: Stream (Maybe Text)
pop = state $ \case
  [] -> (Nothing, [])
  (x:xs) -> (Just x, xs)

peek :: Stream (Maybe Text)
peek = gets $ fmap fst . List.uncons

push :: Text -> Stream ()
push s = modify' (s:)

--------------------------------------------------------------------------------

data ParserResult p r
  = Done r
  | Partial (p r)
  | Empty
  deriving (Functor)

-- | A type class for anything that can be a parsing node in a
-- 'ParseTree'.
class Parser p where
  feedParser :: p r -> Stream (ParserResult p r)

--------------------------------------------------------------------------------
-- Feeding the Tree

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. Once a subtree consumes input, it is replaced with
-- an updated subtree and further traversal ceases.
feed :: Parser p => ParseTree p r -> MaybeT Stream (ParseTree p r)
feed EmptyNode = empty
feed (ValueNode _) = empty
feed (ParseNode parser) = lift (feedParser parser) >>= \case
  Done value      -> pure $ ValueNode value
  Partial parser' -> pure $ ParseNode parser'
  Empty -> empty
feed (MapNode f tree) = MapNode f <$> feed tree
feed (ProdNode f l r) =
  (ProdNode f <$> feed l <*> pure r) <|>
  (ProdNode f l <$> feed r)
feed (SumNode l r) =
  (SumNode <$> feed l <*> pure r) <|>
  (SumNode l <$> feed r)
feed (ManyNode tree) = ProdNode (:) <$> feed tree <*> pure (ManyNode tree)

-- | Repeatedly feed input to the tree using `feed` until either no
-- input is consumed after traversal (e.g. `feed` returns Nothing), or
-- no input remains.
consume :: Parser p => ParseTree p r -> Stream (ParseTree p r)
consume tree = do
  result <- runMaybeT $ feed tree
  case result of
    Just tree' -> do
      isEmpty <- isEmptyStream
      if isEmpty
        then pure tree'
        else consume tree'
    _ -> pure tree
