{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module ParseTree
  ( ParseTree(..)
  , satiate
  , runParseTree
  , parseArguments
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Kind
import           Data.Proxy
import           Data.Text            (Text)

import           Parser
import           Stream
import           Text

-- | 'ParseTree p r' is an expression tree built from parsers of type
-- 'p' which evaluates to a value of type 'r' supplied with the proper
-- input.
data ParseTree (p :: Type -> Type) (r :: Type) where
  -- | Terminal node with no value (abstracts 'empty')
  EmptyNode :: ParseTree p r
  -- | A terminal node with a resolved value (abstracts 'pure')
  ValueNode :: r -> ParseTree p r
  -- | A parser awaiting input
  ParseNode :: p r -> ParseTree p r
  -- | Abstracts 'liftA2' and by extension '(<*>)'
  ProdNode :: (u -> v -> r) -> ParseTree p u -> ParseTree p v -> ParseTree p r
  -- | Abstracts '(<|>)'
  SumNode :: ParseTree p r -> ParseTree p r -> ParseTree p r
  -- | Abstracts 'many' (@MaybeNode False@) and 'some' (@MaybeNode True@)
  ManyNode :: Bool -> ParseTree p r -> ParseTree p [r]

instance Functor p => Functor (ParseTree p) where
  fmap _ EmptyNode          = EmptyNode
  fmap f (ValueNode value)  = ValueNode $ f value
  fmap f (ParseNode parser) = ParseNode $ fmap f parser
  fmap f (ProdNode g l r)   = ProdNode (\u v -> f $ g u v) l r
  fmap f (SumNode l r)      = SumNode (fmap f l) (fmap f r)
  fmap f node               = liftA2 ($) (pure f) node
  -- This takes advantage of the fact that f <$> x = pure f <*> x.

instance Functor p => Applicative (ParseTree p) where
  pure = ValueNode
  liftA2 = ProdNode

instance Functor p => Alternative (ParseTree p) where
  empty = EmptyNode
  (<|>) = SumNode
  many = ManyNode False
  some = ManyNode True

instance HasValency p => HasValency (ParseTree p) where
  valency EmptyNode        = Just 0
  valency (ValueNode _)    = Just 0
  valency (ParseNode p)    = valency p
  valency (ProdNode _ l r) = (+) <$> valency l <*> valency r
  valency (SumNode l r)    = max <$> valency l <*> valency r
  valency (ManyNode _ p)   = case valency p of
                               Just n | n <= 0 -> Just 0
                               _               -> Nothing
  -- In the above case of 'ManyNode _ p', a ManyNode can accept an
  -- arbitrary number of parameters, so the maximum valency is either
  -- infinite or zero depending on whether the valency of 'p' is zero.

instance Resolve p => Resolve (ParseTree p) where
  resolve EmptyNode          = throwError EmptyError
  resolve (ValueNode value)  = pure value
  resolve (ParseNode parser) = resolve parser
  resolve (ProdNode f l r)   = f <$> resolve l <*> resolve r
  resolve (SumNode l r)      = sumResults (resolve l) (resolve r)
  resolve (ManyNode False _) = pure []
  resolve (ManyNode True  p) = pure <$> resolve p
  -- NOTE: If a ManyNode contains a resolvable node, one might expect
  -- the result to be an infinite list (e.g. `resolve $ many
  -- (ValueNode 1)` to give `Right [1,1,1,1,..]`) or for the
  -- computation to diverge (as is the case for `many (Just 1)`).
  -- However, by only attempting at most resolutions of the subtree,
  -- we will get either zero or one results. For example, `resolve $
  -- many (ValueNode 1)` will give `Right []`.
  --
  -- Whether this is the best possible way to handle the situation is
  -- unclear. This avoids infinite loops, but might not be the
  -- expected behavior in some unforseen use-case.

instance Parser p => Render (ParseTree p r) where
  -- special cases
  render (SumNode p (ValueNode _)) = "[" <> render p <> "]"

  render EmptyNode                 = "EMPTY"
  render (ValueNode _)             = "VALUE"
  render (ParseNode parser)        = renderParser parser
  render (ProdNode _ l r)          = render l <> sepProd (Proxy @p) <> render r
  render (SumNode l r)             = render l <> sepSum (Proxy @p) <> render r
  render (ManyNode False p)        = "[" <> render p <> "...]"
  render (ManyNode True p)         = render p <> "..."

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. Once a subtree consumes input, it is replaced with
-- an updated subtree and further traversal ceases.
feed :: Parser p => ParseTree p r -> StreamParser (Token p) (ParseTree p r)
feed EmptyNode = empty
feed (ValueNode _) = empty
feed (ParseNode parser) = ValueNode <$> feedParser parser
feed (ProdNode f l r) =
  (ProdNode f <$> feed l <*> pure r) <|>
  (ProdNode f l <$> feed r)
feed (SumNode l r) = feed l <|> feed r
feed (ManyNode _ tree) =
  ProdNode (:)
  <$> feed tree
  <*> pure (ManyNode False tree)

-- | Repeatedly feed input to the tree using `feed` until no input is
-- no longer consumed.
satiate :: Parser p => ParseTree p r -> StreamParser (Token p) (ParseTree p r)
satiate tree = do
  result <- optional $ feed tree
  case result of
    Just tree' -> satiate tree'
    Nothing    -> pure tree

runParseTree
  :: Parser p
  => ParseTree p r
  -> [Token p]
  -> Either Builder (r, [Token p])
runParseTree tree args =
  case runStreamParser (satiate tree) [] args of
    ParseEmpty _ _            -> Left "empty"
    ParseError contexts' err  -> Left $ errorInContext contexts' err
    ParseResult _ args' tree' ->
      case resolve tree' of
        Right value -> Right (value, args')
        Left err ->
          case args' of
            (arg:_) -> Left $ "unexpected " <> render arg
            _       -> Left $ render err

parseArguments
  :: Parser p
  => ParseTree p r
  -> [Text]
  -> Either Builder (r, [Token p])
parseArguments tree = runParseTree tree . parseTokens
