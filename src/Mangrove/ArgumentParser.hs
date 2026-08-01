{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mangrove.ArgumentParser
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A complete (but generic) argument parsing interface.
-}
module Mangrove.ArgumentParser
  ( Result(..)
  , runArgumentParser
  , runArgumentParser'
  ) where

import           Data.Text          (Text)

import           Mangrove.ParseTree
import           Mangrove.Resolve
import           Mangrove.Text

-- | Represents the final result of a parsing operation. Each
-- constructor represents a potential exit point.
data Result tok r
  = Success [Text] r
  | Failure [tok] Builder
  | HelpRequest [tok]
  deriving (Eq, Show)

-- | Satiate a 'ParseTree' with all the input it can consume, then
-- attempt to evaluate it.
runArgumentParser
  :: Scheme s
  => ParseTree s r
  -> [Text]
  -> Result (Token s) r
runArgumentParser tree args =
  runArgumentParser' tree StreamState
  { streamContent = args
  , streamContext = []
  , streamEscaped = False
  }

-- | This is the same thing as 'runArgumentParser', but accepts a
-- custom 'StreamState' as the starting state.
runArgumentParser'
  :: Scheme s
  => ParseTree s r
  -> StreamState s
  -> Result (Token s) r
runArgumentParser' tree =
  runStreamParser (satiate tree) handler
  where
    _onFailure = Failure . streamContext
    _onSuccess state' tree' =
      case (streamContent state', resolve tree') of
        (leftovers, Value result) -> Success leftovers result
        ([], EmptyError)          -> _onFailure state' "empty"
        ([], ExpectedError es)    -> _onFailure state' $ renderExpectedError es
        (token:_, _)              -> _onFailure state' $ "unexpected " <> render token
    handler = StreamHandler
      { onSuccess = _onSuccess
      , onFailure = _onFailure
      , onEmpty = flip _onFailure "empty"
      , onHelpRequest = HelpRequest . streamContext
      }
