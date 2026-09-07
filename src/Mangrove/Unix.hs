{-# LANGUAGE GADTs #-}

{-|
Module      : Mangrove.Unix
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

This module contains the building blocks for creating UNIX-style
command line parsers. Parsers are intended to be combined using the
standard combinators in "Control.Applicative".

The functions for running parsers live in the "Mangrove.Parser" module
and are re-exported by the "Mangrove" module alongside other useful
symbols.
-}

module Mangrove.Unix
  ( -- * Types
    UnixScheme
  , SubScheme
  , UnixParser
  , SubParser
  , Flag(..)
  , TextParser(..)
  , DefaultParser(..)

    -- * Tree-building Combinators
  , parameter
  , option
  , optionPure
  , switch
  , requestOption
  , command
  , subparameter
  , suboption

  -- ** Help Options
  , addHelpOptions
  ) where

import           Control.Applicative
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Text            (Text)

import           Mangrove.ParseTree
import           Mangrove.Scheme.Sub  (SubParser, SubScheme)
import qualified Mangrove.Scheme.Sub  as Sub
import           Mangrove.Scheme.Unix
import           Mangrove.Stream
import           Mangrove.TextParser

--------------------------------------------------------------------------------
-- Tree-building Combinators

-- | Create a parameter parser from a t'TextParser'.
parameter
  :: TextParser r
  -> UnixParser r
parameter = ParseNode . Parameter

-- | Define a general CLI option.
option
  :: NonEmpty Flag
  -> Text
  -> SubParser r
  -> UnixParser r
option flags help = ParseNode . Option (OptionInfo flags help)

-- | Define a CLI option which takes no parameter and produces a pure value.
optionPure
  :: NonEmpty Flag
  -> Text
  -> a
  -> UnixParser a
optionPure flags help = option flags help . pure

-- | Define a CLI option which produces 'True' if present and 'False'
-- otherwise.
switch :: NonEmpty Flag -> Text -> UnixParser Bool
switch flags help = optionPure flags help True <|> pure False

-- | A special option that triggers a request for information.
--
-- When a request option is encountered in the command line, a
-- "request" is raised and parsing is abandoned in favor of yielding a
-- human-readable response.
requestOption
  :: NonEmpty Flag
  -> Text
  -> RequestType
  -> UnixParser a
requestOption flags help = ParseNode . RequestOption (OptionInfo flags help)

-- | Define a CLI subcommand with it's own parsing subtree.
command
  :: NonEmpty Text
  -> Text
  -> UnixParser r
  -> UnixParser r
command cmds help = ParseNode . Command (CommandInfo cmds help)

-- | Define a subparameter to a CLI option.
subparameter :: TextParser a -> SubParser a
subparameter = ParseNode . Sub.Parameter

-- | Define a suboption to a CLI option.
suboption :: Text -> TextParser a -> SubParser a
suboption key = ParseNode . Sub.Option key
