{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

{-|
Module      : Mangrove.ArgumentParser
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

This module contains an API (types and functions) for running argument parsers.
-}
module Mangrove.ArgumentParser
  ( -- * Types
    ProgramInfo(..)
  , Result(..)

    -- * Silent Parsers
  , runSilentParser
  , runSilentParser'

    -- * Helpful Parsers
  , runHelpfulParser
  , runHelpfulParser'
  , runHelpfulParser_

    -- * Generic Parsers
  , runArgumentParser
  , runArgumentParser'
  ) where

import           Data.Text          (Text)

import           Mangrove.ParseTree
import           Mangrove.Resolve
import           Mangrove.Text

-- | Program metadata for displaying help output.
data ProgramInfo = ProgramInfo
  { programName :: !Text -- ^ The program name
  , programDesc :: !Text -- ^ A description of the program
  } deriving (Show)

-- | The results of a parsing operation.
--
-- Only parsing schemes that support generating help output will yield
-- 'Help' values.
data Result s r where
  Success :: ![Text] -> !r -> Result s r
  Failure :: !Text -> Result s r
  Help :: SupportsHelp s => !Text -> Result s r

deriving instance Show r => Show (Result s r)
deriving instance Eq r => Eq (Result s r)

-- | Create a default initial 'StreamState' from a list of arguments.
argsToState :: [Text] -> StreamState s
argsToState args = StreamState args [] False

-- | Attempt to parse a value of type @r@ from a list of arguments,
-- where the parser @ParseTree s r@ doesn't support help output.
runSilentParser
  :: (Scheme s, HelpSupport s ~ 'Silent)
  => ParseTree s r
  -> [Text] -- ^ Input arguments
  -> Result s r
runSilentParser tree = runSilentParser' tree . argsToState

-- | A more general form of 'runSilentParser' that accepts a custom
-- 'StreamState' as the starting state.

runSilentParser'
  :: (Scheme s, HelpSupport s ~ 'Silent)
  => ParseTree s r
  -> StreamState s
  -> Result s r
runSilentParser' tree state =
  runArgumentParser' tree state Success Failure NoHelp

-- | Attempt to parse a value of type @r@ from a list of arguments,
-- where the parser @ParseTree s r@ supports help output.
runHelpfulParser
  :: SupportsHelp s
  => ProgramInfo
  -> ParseTree s r
  -> [Text]
  -> Result s r
runHelpfulParser info tree = runHelpfulParser' info tree . argsToState

-- | A more general form of 'runHelpfulParser' that accepts a custom
-- 'StreamState' as the starting state.
runHelpfulParser'
  :: SupportsHelp s
  => ProgramInfo
  -> ParseTree s r
  -> StreamState s
  -> Result s r
runHelpfulParser' info tree state =
  runArgumentParser' tree state Success Failure (OnHelp _onHelpRequest)
  where
    _onHelpRequest state' =
      Help $ makeHelpInfo tree (streamContext state') (programName info) (programDesc info)

-- | A variant of 'runHelpfulParser' that treats help requests as
-- failures.
runHelpfulParser_
  :: SupportsHelp s
  => ParseTree s r
  -> [Text]
  -> Result s r
runHelpfulParser_ tree args =
  runArgumentParser' tree (argsToState args) Success Failure (OnHelp _onHelpRequest)
  where
    _onHelpRequest state' = Failure $ renderText $
      renderError (streamContext state') "help requested"

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
