{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
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
import qualified Data.List              as List
import           Data.Proxy
import           Data.Text              (Text)
import qualified Data.Text.Lazy.Builder as TLB

import           Parser
import           Stream
import           Text

-- | 'ParseTree p r' is an expression tree built from parsers of type
-- 'p' which evaluates to a value of type 'r' supplied with the proper
-- input.
data ParseTree (p :: Type -> Type) (r :: Type) where
  -- | Terminal node with no value
  EmptyNode :: ParseTree p r
  -- | Terminal node with a resolved value
  ValueNode :: r -> ParseTree p r
  -- | A parser awaiting input
  ParseNode :: p r -> ParseTree p r
  -- | Abstracts fmap
  MapNode :: (a -> r) -> ParseTree p a -> ParseTree p r
  -- | Abstracts liftA2
  ProdNode :: (u -> v -> r) -> ParseTree p u -> ParseTree p v -> ParseTree p r
  -- | Abstracts (<|>)
  SumNode :: ParseTree p r -> ParseTree p r -> ParseTree p r
  -- | Abstracts many
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
  some p = ProdNode (:) p (ManyNode p)

instance HasValency p => HasValency (ParseTree p) where
  valency EmptyNode        = Just 0
  valency (ValueNode _)    = Just 0
  valency (ParseNode p)    = valency p
  valency (MapNode _ p)    = valency p
  valency (ProdNode _ l r) = (+) <$> valency l <*> valency r
  valency (SumNode l r)    = max <$> valency l <*> valency r
  valency (ManyNode p)     = case valency p of
                               Just n | n <= 0 -> Just 0
                               _               -> Nothing

instance Resolve p => Resolve (ParseTree p) where
  resolve EmptyNode          = throwError "empty"
  resolve (ValueNode value)  = pure value
  resolve (ParseNode parser) = resolve parser
  resolve (MapNode f p)      = fmap f $ resolve p
  resolve (ProdNode f l r)   = f <$> resolve l <*> resolve r
  resolve (SumNode l r)      = resolve l <> resolve r
  resolve (ManyNode _)       = pure []
  -- TODO: What if the ManyNode contains a resolvable node (e.g.
  -- `ManyNode (ValueNode 5)`)? Handling it this way avoids infinite
  -- loops, but might not be the expected behavior.

instance Parser p => Render (ParseTree p r) where
  -- special cases
  render (SumNode p (ValueNode _)) = "[" <> render p <> "]"

  render EmptyNode                 = "EMPTY"
  render (ValueNode _)             = "VALUE"
  render (ParseNode parser)        = renderParser parser
  render (MapNode _ p)             = render p
  render (ProdNode _ l r)          = render l <> sepProd (Proxy @p) <> render r
  render (SumNode l r)             = render l <> sepSum (Proxy @p) <> render r
  render (ManyNode p)              = "[" <> render p <> "...]"

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. Once a subtree consumes input, it is replaced with
-- an updated subtree and further traversal ceases.
feed :: Parser p => ParseTree p r -> StreamParser (Token p) (ParseTree p r)
feed EmptyNode = empty
feed (ValueNode _) = empty
feed (ParseNode parser) = ValueNode <$> feedParser parser
feed (MapNode f tree) = MapNode f <$> feed tree
feed (ProdNode f l r) =
  (ProdNode f <$> feed l <*> pure r) <|>
  (ProdNode f l <$> feed r)
feed (SumNode l r) =
  (SumNode <$> feed l <*> pure r) <|>
  (SumNode l <$> feed r)
feed (ManyNode tree) =
  ProdNode (:)
  <$> feed tree
  <*> pure (ManyNode tree)

-- | Repeatedly feed input to the tree using `feed` until no input is
-- no longer consumed.
satiate :: Parser p => ParseTree p r -> StreamParser (Token p) (ParseTree p r)
satiate tree = do
  result <- optional $ feed tree
  case result of
    Just tree' -> satiate tree'
    Nothing    -> pure tree

runParseTree
  :: (Alternative m, MonadError TLB.Builder m, Parser p)
  => ParseTree p r
  -> [Text]
  -> m (r, [Token p])
runParseTree tree args =
  case runStreamParser (satiate tree) [] (parseTokens args) of
    ParseResult _ args' tree' -> (,) <$> liftEither (resolve tree') <*> pure args'
    ParseEmpty _ _            -> throwError "empty subparser"
    ParseError contexts' err  -> throwWithContext contexts' err
  where
    throwWithContext contexts =
      throwError
      . mconcat
      . List.reverse
      . List.intersperse ": "
      . (: contexts)

parseArguments
  :: Parser p
  => ParseTree p r
  -> [Text]
  -> Either TLB.Builder (r, [Token p])
parseArguments tree args = runExcept $ do
  (result, tokens) <- runParseTree tree args
  pure (result, tokens)
