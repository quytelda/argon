{-# LANGUAGE GADTs        #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import qualified Data.List                 as List
import           Data.Text                 (Text)
import qualified Data.Text                 as T

data ParseTree p r where
  EmptyNode :: ParseTree p r
  ValueNode :: r -> ParseTree p r
  ParseNode :: p r -> ParseTree p r
  MapNode :: (a -> r) -> ParseTree p a -> ParseTree p r
  ProdNode :: (u -> v -> r) -> ParseTree p u -> ParseTree p v -> ParseTree p r
  SumNode :: ParseTree p r -> ParseTree p r -> ParseTree p r
  ManyNode :: ParseTree p r -> ParseTree p [r]

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

data Token
  = LongOption Text
  | Argument Text
  deriving (Show)

class Accepts a where
  accepts :: a -> Token -> Bool

instance Accepts (ParseTree p a) where
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

type TreeParser p r = StateT (ParseTree p r) Stream

runTreeParser :: TreeParser p r a -> ParseTree p r -> [Text] -> ((a, ParseTree p r), [Text])
runTreeParser action = runState . runStateT action

-- | Execute a TreeParser computation on a different tree.
withTree :: TreeParser p v a -> ParseTree p v -> TreeParser p r (a, ParseTree p v)
withTree action = lift . runStateT action

parseToken :: Text -> TreeParser p r Token
parseToken (T.stripPrefix "--" -> Just name) = pure $ LongOption name
parseToken content                           = pure $ Argument content

popToken :: TreeParser p r (Maybe Token)
popToken = lift pop >>= \case
  Just s -> Just <$> parseToken s
  Nothing -> pure Nothing

peekToken :: TreeParser p r (Maybe Token)
peekToken = lift peek >>= \case
  Just s -> Just <$> parseToken s
  Nothing -> pure Nothing
