{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolymorphicComponents     #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

{-|
Module      : Mangrove.Parser
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A "scheme" is a system of parsers and tokens. It determines the method
by which argument strings are separated. It parses a sequence of
arguments into tokens and values.
-}
module Mangrove.Parser
  ( -- * Feeding Trees
    satiate

    -- * Parsing Schemes
  , Scheme(..)
  , ProgramInfo(..)
  , SupportsResponse(..)
  ) where

import           Control.Applicative
import           Data.Kind
import           Data.Text              (Text)
import           Data.Version

import           Mangrove.ParseTree
import           Mangrove.Resolve
import           Mangrove.Stream
import           Mangrove.Token

--------------------------------------------------------------------------------
-- Parsing Schemes

-- | A scheme is a system of parsers and tokens. It parses a sequence
-- of arguments into tokens and values.
class (Functor s, Resolve s, HasTokens s) => Scheme (s :: Type -> Type) where
  -- | Parse special control arguments that don't represent tokens in
  -- the scheme, but control aspects of how parsing proceeds (e.g.
  -- escaping).
  parseSpecials :: StreamParser s ()
  parseSpecials = pure ()

  -- | 'activate' tries to run a parser on the current input. If the
  -- parser doesn't apply, it consumes nothing and returns empty. If
  -- it does apply, it consumes the relevant input and returns a
  -- result.
  activate :: s r -> StreamParser s r

-- | Program metadata for displaying help output.
data ProgramInfo (s :: Type -> Type) = ProgramInfo
  { programName    :: !Text -- ^ The program name
  , programVersion :: !Version -- ^ The program version
  , programDesc    :: !Text -- ^ A description of the program
  } deriving (Show)

-- | A class for schemes that support human-readable responses to
-- requests for help or version information.
class (Scheme s, RequestSupport s ~ 'True) => SupportsResponse s where
  makeVersionInfo :: ProgramInfo s -> Text
  makeHelpInfo :: ParseTree s r -> [Token s] -> ProgramInfo s -> Text

--------------------------------------------------------------------------------

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. When a subtree successfully consumes input, it is
-- replaced with an updated subtree and the traversal ceases.
feed :: Scheme s => ParseTree s r -> StreamParser s (ParseTree s r)
feed EmptyNode = empty
feed (ValueNode _) = empty
feed (ParseNode parser) = ValueNode <$> activate parser
feed (ProdNode f l r) =
  (ProdNode f <$> feed l <*> pure r) <|>
  (ProdNode f l <$> feed r)
feed (SumNode l r) = feed l <|> feed r
feed (ManyNode _ tree) =
  ProdNode (:)
  <$> feed tree
  <*> pure (ManyNode False tree)

-- | Repeatedly traverse the tree, each time activating the first
-- parser that can consume available input, until no more input can be
-- consumed.
satiate :: Scheme s => ParseTree s r -> StreamParser s (ParseTree s r)
satiate tree = do
  parseSpecials
  result <- optional $ feed tree
  case result of
    Just tree' -> satiate tree'
    Nothing    -> pure tree
