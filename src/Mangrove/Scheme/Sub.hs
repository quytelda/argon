{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
Module      : Mangrove.Scheme.Sub
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A parsing scheme for Unix-style subarguments (i.e. arguments passed as
subarguments to an option).
-}
module Mangrove.Scheme.Sub
  ( -- * Types
    SubScheme(..)
  , Token(..)
  , SubParser

    -- * Properties
  , hasSubOptions
  ) where

import           Control.Applicative
import           Data.Text           (Text)

import           Mangrove.Parser
import           Mangrove.Resolve
import           Mangrove.Separable
import           Mangrove.Text
import           Mangrove.TextParser
import           Mangrove.Valency

-- | Parsers for subarguments of an option (e.g. @--option key=value@).
data SubScheme r
  = Parameter (TextParser r) -- ^ Parses freeform arguments
  | Option Text (TextParser r) -- ^ Suboptions have the form "KEY=VALUE"
  deriving (Functor)

instance Valency SubScheme where
  valency _ = Just 1

instance Resolve SubScheme where
  resolve (Parameter (TextParser hint _)) =
    ExpectedError [render hint]
  resolve (Option key (TextParser hint _)) =
    ExpectedError [render key <> "=" <> render hint]

instance Separable SubScheme where
  separate s = Exhibit (Just s) []

instance Scheme SubScheme where
  data Token SubScheme
    = SubAssoc Text Text -- ^ A "KEY=VALUE" argument
    | SubArgument Text -- ^ A standard freeform argument
    deriving (Eq, Show)

  delimiter _ = ','

  activate parser = do
    next <- peek
    escaped <- getEscaped
    case (parser, keyEqualsValue next) of
      (Parameter tp, Nothing) ->
        withContext (SubArgument next) $
        pop_ *> runTextParser tp next
      (Option _ _, Nothing) ->
        empty
      (Option key tp, Just (k,v))
        | not escaped && key == k ->
          withContext (SubAssoc k v) $
          pop_ *> runTextParser tp v
      (Parameter tp, Just _)
        | escaped ->
          withContext (SubArgument next) $
          pop_ *> runTextParser tp next
        | otherwise ->
          empty
      _ -> empty

  usageInfo (Parameter tp)  = render $ parserHint tp
  usageInfo (Option key tp) = render key <> "=" <> render (parserHint tp)

instance Render (Token SubScheme) where
  render (SubAssoc key value) = render key <> "=" <> render value
  render (SubArgument value)  = render value

-- | Type alias for SubScheme parse trees.
type SubParser = ParseTree SubScheme

-- | Check whether a parse tree contains any suboption parsers. This
-- allows us to determine if we need to parse "KEY=VALUE" pairs.
hasSubOptions :: ParseTree SubScheme r -> Bool
hasSubOptions EmptyNode                 = False
hasSubOptions (ValueNode _)             = False
hasSubOptions (ParseNode (Parameter _)) = False
hasSubOptions (ParseNode (Option _ _))  = True
hasSubOptions (ProdNode _ l r)          = hasSubOptions l || hasSubOptions r
hasSubOptions (SumNode l r)             = hasSubOptions l || hasSubOptions r
hasSubOptions (ManyNode _ tree)         = hasSubOptions tree
