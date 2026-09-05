{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolymorphicComponents     #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : Mangrove.ParseTree
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A 'ParseTree' is a tree-shaped parser that "filter-feeds" on a stream
of arguments, collecting inputs at the leaves and feeding the results
up the tree for processing. 'ParseTree's are parameterized by the
parser scheme that determines the kind of inputs it accepts.
-}

module Mangrove.ParseTree
  ( -- * Parse Trees
    ParseTree(..)
  , isProduct
  , isSum
  , isOptional
  , isChoice
  ) where

import           Control.Applicative
import           Data.Kind
import           Data.Proxy

import           Mangrove.Resolve
import           Mangrove.Stream
import           Mangrove.Text
import           Mangrove.Valency

-- | `ParseTree scheme r` is an expression tree composed of parsers
-- from scheme @scheme@ which evaluates to a value of type @r@ when
-- supplied with the proper input.
data ParseTree (scheme :: Type -> Type) (r :: Type) where
  -- | Terminal node with no value (abstracts 'empty')
  EmptyNode :: ParseTree scheme r
  -- | A terminal node with a resolved value (abstracts 'pure')
  ValueNode :: !r -> ParseTree scheme r
  -- | A parser awaiting input
  ParseNode :: scheme r -> ParseTree scheme r
  -- | Abstracts 'liftA2' and by extension '(<*>)'
  ProdNode :: !(u -> v -> r) -> ParseTree scheme u -> ParseTree scheme v -> ParseTree scheme r
  -- | Abstracts '(<|>)'
  SumNode :: ParseTree scheme r -> ParseTree scheme r -> ParseTree scheme r
  -- | Abstracts 'many' (@MaybeNode False@) and 'some' (@MaybeNode True@)
  ManyNode :: !Bool -> ParseTree scheme r -> ParseTree scheme [r]

instance (forall a. Show (s a)) => Show (ParseTree s r) where
  showsPrec _ EmptyNode = showString "EmptyNode"
  showsPrec p (ValueNode _) =
    showParen (p >= 10)
    $ showString "ValueNode _"
  showsPrec p (ParseNode s) =
    showParen (p >= 10)
    $ showString "ParseNode "
    . showsPrec 11 s
  showsPrec p (ProdNode _ l r) =
    showParen (p >= 10)
    $ showString "ProdNode _ "
    . showsPrec 11 l
    . showString " "
    . showsPrec 11 r
  showsPrec p (SumNode l r) =
    showParen (p >= 10)
    $ showString "SumNode "
    . showsPrec 11 l
    . showString " "
    . showsPrec 11 r
  showsPrec p (ManyNode b t) =
    showParen (p >= 10)
    $ showString "ManyNode "
    . showsPrec 11 b
    . showString " "
    . showsPrec 11 t

instance Functor p => Functor (ParseTree p) where
  fmap _ EmptyNode          = EmptyNode
  fmap f (ValueNode value)  = ValueNode $ f value
  fmap f (ParseNode parser) = ParseNode $ fmap f parser
  fmap f (ProdNode g l r)   = ProdNode (\u v -> f $ g u v) l r
  fmap f (SumNode l r)      = SumNode (fmap f l) (fmap f r)
  fmap f node               = ProdNode ($) (pure f) node
  -- This takes advantage of the fact that f <$> x = pure f <*> x.

instance Functor p => Applicative (ParseTree p) where
  pure = ValueNode
  liftA2 = ProdNode

instance Functor p => Alternative (ParseTree p) where
  empty = EmptyNode
  (<|>) = SumNode
  many = ManyNode False
  some = ManyNode True

instance Valency s => Valency (ParseTree s) where
  valency EmptyNode         = Just 0
  valency (ValueNode _)     = Just 0
  valency (ParseNode p)     = valency p
  valency (ProdNode _ l r)  = (+) <$> valency l <*> valency r
  valency (SumNode l r)     = max <$> valency l <*> valency r
  valency (ManyNode _ tree) =
    case valency tree of
      Just n | n <= 0 -> Just 0
      _               -> Nothing -- i.e. infinity
  -- In the above case of 'ManyNode _ p', a ManyNode can accept an
  -- arbitrary number of parameters, so the maximum valency is either
  -- infinite or zero depending on whether the valency of 'p' is zero.

  -- Since ParseTrees themselves don't accept inputs, we can provide a
  -- slightly more efficient implementation of nullary.
  nullary EmptyNode         = True
  nullary (ValueNode _)     = True
  nullary (ParseNode p)     = nullary p
  nullary (ProdNode _ l r)  = nullary l && nullary r
  nullary (SumNode l r)     = nullary l && nullary r
  nullary (ManyNode _ tree) = nullary tree

instance Resolve s => Resolve (ParseTree s) where
  resolve EmptyNode          = EmptyError
  resolve (ValueNode value)  = pure value
  resolve (ParseNode parser) = resolve parser
  resolve (ProdNode f l r)   = f <$> resolve l <*> resolve r
  resolve (SumNode l r)      = resolve l <|> resolve r
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

-- | Is this a 'ProdNode'?
isProduct :: ParseTree s r -> Bool
isProduct (ProdNode {}) = True
isProduct _             = False

-- | Is this a 'SumNode'?
isSum :: ParseTree s r -> Bool
isSum (SumNode {}) = True
isSum _            = False

-- | Does this subtree accept optional input?
isOptional :: Valency s => ParseTree s r -> Bool
isOptional (SumNode l (ValueNode _)) = not $ nullary l
isOptional (ManyNode False p)        = not $ nullary p
isOptional _                         = False

-- | Is this a 'SumNode' a choice between two different (non-empty)
-- inputs?
isChoice :: Valency s => ParseTree s r -> Bool
isChoice (SumNode l r) = not (nullary l) && not (nullary r)
isChoice _             = False

instance (Valency s, ParserInfo s, forall a. Render (s a)) => Render (ParseTree s r) where
  -- special cases
  render n@(SumNode l _)
    | isOptional n = renderDelimitedIf brackets (not . isOptional) l

  render (ParseNode parser) = render parser
  render (ProdNode _ l r)
    | nullary l && nullary r = ""
    | nullary l = _render r
    | nullary r = _render l
    | otherwise = _render l <> render sep <> _render r
    where
      _render = renderDelimitedIf braces isChoice
      sep = delimiter (Proxy @s)
  render (SumNode l r)
    | nullary l && nullary r = ""
    | nullary l = _render r
    | nullary r = _render l
    | otherwise = _render l <> "|" <> _render r
    where
      _render = renderDelimitedIf braces isProduct
  render (ManyNode required p) = wrap $ render p <> "..."
    where
      wrap = if required
             then braces
             else brackets

  -- Constant nodes that don't accept input have no usage.
  render _ = ""
