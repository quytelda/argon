module Argon where

import           CliParser
import           ParseTree
import           SubParser
import           TextParser

import           Control.Applicative
import           Data.List.NonEmpty  (NonEmpty)
import           Data.Text           (Text)

-- | Define a command line parameter (i.e. a non-option).
parameter :: TextParser a -> ParseTree CliParser a
parameter = ParseNode . CliParameter

-- | Define a standard CLI option triggered by one or more flags.
option
  :: NonEmpty Flag
  -> Text
  -> ParseTree SubParser a
  -> ParseTree CliParser a
option flags help = ParseNode . CliOption (OptionInfo flags help)

-- | Define a CLI option which takes no parameter and produces a pure value.
optionPure
  :: NonEmpty Flag
  -> Text
  -> a
  -> ParseTree CliParser a
optionPure flags help = ParseNode . CliOption (OptionInfo flags help) . pure

-- | Define a CLI option that takes one simple subparameter.
optionUnary
  :: NonEmpty Flag
  -> Text
  -> TextParser a
  -> ParseTree CliParser a
optionUnary flags help = option flags help . subparameter

-- | Define a CLI option which produces 'True' if present and 'False'
-- otherwise.
switch :: NonEmpty Flag -> Text -> ParseTree CliParser Bool
switch flags help = optionPure flags help True <|> pure False

-- | Define a CLI subcommand with it's own parsing subtree.
command
  :: NonEmpty Text
  -> Text
  -> ParseTree CliParser r
  -> ParseTree CliParser r
command cmds help = ParseNode . CliCommand (CommandInfo cmds help)

-- | Define a subparameter to a CLI option.
subparameter :: TextParser a -> ParseTree SubParser a
subparameter = ParseNode . SubParameter

-- | Define a suboption to a CLI option.
suboption :: Text -> TextParser a -> ParseTree SubParser a
suboption key = ParseNode . SubAssoc key
