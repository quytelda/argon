{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mangrove.Unix
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

An API for defining, constructing, and running Unix-style command line
parsers.
-}

module Mangrove.Unix
  ( -- * Types
    UnixParser
  , SubParser

    -- * Running Parsers
  , parseArguments

    -- * Tree-building Combinators
  , parameter
  , option
  , optionPure
  , switch
  , command
  , subparameter
  , suboption

  -- Re-exports
  , TextParser(..)
  , defaultParser
  , addHelpOptions
  ) where

import           Control.Applicative
import           Data.List.NonEmpty      (NonEmpty)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           System.Environment
import           System.Exit
import           System.IO

import           Mangrove.ArgumentParser
import           Mangrove.ParseTree
import           Mangrove.Scheme.Sub     (SubParser)
import qualified Mangrove.Scheme.Sub     as Sub
import           Mangrove.Scheme.Unix
import           Mangrove.Separable
import           Mangrove.Text
import           Mangrove.TextParser

-- | Parse the command line arguments passed to the program, then
-- invoke the program's entrypoint with the results of the parsing. If
-- parsing fails, we instead display an error to stderr and exit.
-- Alternatively, if help was requested, we abandon parsing and print
-- the relevant help output to stdout, then exit without indicating an
-- error.
parseArguments
  :: UnixParser r
  -> Text -- ^ Program Name
  -> Text -- ^ Program Description
  -> (r -> IO a) -- ^ Program Entrypoint
  -> IO a
parseArguments tree name description action = do
  args <- map T.pack <$> getArgs
  case runArgumentParser tree args of
    Success [] result -> action result
    Success (token:_) _ -> do
      hPutBuilder stderr $ "unexpected " <> render token <> "\n"
      exitFailure
    Failure contexts err -> do
      hPutBuilder stderr $ renderError contexts err <> "\n"
      exitFailure
    HelpRequest contexts -> do
      putBuilder
        $ "Usage:\n"
        <> renderUsages tree <> "\n"
        <> render description <> "\n"
        <> renderHelp tree contexts
      exitSuccess
  where
    renderUsageLine s = render name <> " " <> render s <> "\n"
    renderUsages = foldMap renderUsageLine . exhibitToList . separate

--------------------------------------------------------------------------------
-- Tree-building Combinators

-- | Create a parameter parser from a 'TextParser'.
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
optionPure flags help = ParseNode . Option (OptionInfo flags help) . pure

-- | Define a CLI option which produces 'True' if present and 'False'
-- otherwise.
switch :: NonEmpty Flag -> Text -> UnixParser Bool
switch flags help = optionPure flags help True <|> pure False

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
