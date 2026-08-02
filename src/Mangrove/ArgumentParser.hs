{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mangrove.ArgumentParser
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A complete (but generic) argument parsing interface.
-}
module Mangrove.ArgumentParser
  ( runArgumentParser
  , runArgumentParser'
  ) where

import           Data.Text          (Text)

import           Mangrove.ParseTree
import           Mangrove.Resolve
import           Mangrove.Text

-- | Satiate a 'ParseTree' with all the input it can consume, then
-- attempt to evaluate it.
runArgumentParser
  :: Scheme s
  => ParseTree s r
  -> [Text] -- ^ Input arguments
  -> ([Text] -> r -> a) -- ^ Success handler
  -> (Text -> a) -- ^ Failure handler
  -> HelpHandler s a -- ^ Help request handler
  -> a
runArgumentParser tree args =
  runArgumentParser' tree StreamState
  { streamContent = args
  , streamContext = []
  , streamEscaped = False
  }

-- | A more general form of 'runArgumentParser' that accepts a custom
-- 'StreamState' as the starting state.
runArgumentParser'
  :: Scheme s
  => ParseTree s r
  -> StreamState s -- ^ Initial stream state
  -> ([Text] -> r -> a) -- ^ Success handler
  -> (Text -> a) -- ^ Failure handler
  -> HelpHandler s a -- ^ Help request handler
  -> a
runArgumentParser' tree state cok cerr hhelp =
  runStreamParser (satiate tree) handler state
  where
    _onFailure state' = cerr . renderText . renderError (streamContext state')
    _onSuccess state' tree' =
      case (streamContent state', resolve tree') of
        (leftovers, Value result) -> cok leftovers result
        ([], EmptyError)          -> _onFailure state' "empty"
        ([], ExpectedError es)    -> _onFailure state' $ renderExpectedError es
        (token:_, _)              -> _onFailure state' $ "unexpected " <> render token
    handler = StreamHandler
      { onSuccess = _onSuccess
      , onFailure = _onFailure
      , onEmpty = flip _onFailure "empty"
      , onHelpRequest = hhelp
      }
