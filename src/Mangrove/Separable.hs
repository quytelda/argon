{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Mangrove.Separable
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Tools for decomposing parsers into different modal subparsers.
-}
module Mangrove.Separable
  ( Separable(..)
  , Modal(..)
  , usesTerseOutput
  , Exhibit(..)
  , exhibitToList
  ) where

import           Data.Kind
import           Data.Maybe

-- | A modal branch of a parser or parse tree is a particular subtree
-- that, when triggered, excludes the rest of the tree from receiving
-- input. This can happen because the parsing context changed (for
-- example, when a command is recognized) or when the parsing is
-- exited entirely (for example, when a help option is encountered).
data Modal a = Modal !Bool a
  deriving (Functor)

instance Applicative Modal where
  pure = Modal False
  Modal terse1 f <*> Modal terse2 x = Modal (terse1 && terse2) (f x)

-- | Modal trees that exit the parsing flow entirely have no
-- opportunity to make use of optional parsers, since their values
-- will never be evaluated. Thus, we mark those trees as having
-- "terse" output so that we can omit the optional subtrees when
-- generating help information.
usesTerseOutput :: Modal a -> Bool
usesTerseOutput (Modal terseOutput _) = terseOutput

-- | A representation of an object whose modal sub-components have
-- been split off for the purpose of better help output.
--
-- Every parser and parse tree can be decomposed into one regular tree
-- and a list of modal trees.
data Exhibit a = Exhibit (Maybe a) [Modal a]
  deriving (Functor)

-- | Convert an t'Exhibit' to a regular list of regular and modal
-- components.
exhibitToList :: Exhibit a -> [a]
exhibitToList (Exhibit mnorm modals) =
  maybeToList mnorm <> [t | Modal _ t <- modals]

-- | A 'Separable' parser is one that can be decomposed into regular
-- and modal subparsers.
--
-- We do this so that we can render usage information for each parser
-- mode separately. This makes the usage of complex commands
-- significantly easier to read.
--
-- NOTE: Decomposed subparsers are intended for display purposes
-- (hence the t'Exhibit' type). Trying to parse input with them is
-- likely to fail.
class Functor s => Separable (s :: Type -> Type) where
  separate :: s r -> Exhibit (s r)
