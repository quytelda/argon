{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Mangrove.Parser
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

High-level parsing functions
-}
module Mangrove.Parser
  ( -- * Standard Interface
    parseArguments

    -- * Types
  , Result(..)

    -- * Pure Interface
    -- ** Helpful Parsers
  , runHelpfulParser
  , runHelpfulParser'
  , runHelpfulParser_

    -- ** Silent Parsers
  , runSilentParser
  , runSilentParser'

    -- ** General Parsers (CPS)
  , runArgumentParser
  , runArgumentParser'

    -- * Feeding Parser Trees
  , satiate
  ) where

import           Control.Applicative
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.Environment
import           System.Exit
import           System.IO

import           Mangrove.ParseTree
import           Mangrove.Resolve
import           Mangrove.Scheme
import           Mangrove.Stream
import           Mangrove.Text

--------------------------------------------------------------------------------
-- Feeding ParseTrees

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

--------------------------------------------------------------------------------
-- Running Parsers

-- | The results of a parsing operation.
--
-- Only parsing schemes that support generating responses can use the
-- 'Response' constructor.
data Result s r where
  -- | A successful parsing operation yields a list of leftover
  -- arguments and a result value.
  Success :: ![Text] -> !r -> Result s r
  -- | A failed parsing operation yields an error message.
  Failure :: !Text -> Result s r
  -- | A request for information yields a human-readable response (for
  -- parsers that support it).
  Response :: SupportsResponse s => !Text -> Result s r

deriving instance Show r => Show (Result s r)
deriving instance Eq r => Eq (Result s r)

-- | Create a default initial t'StreamState' from a list of arguments.
argsToState :: [Text] -> StreamState s
argsToState args = StreamState args [] False

-- | Attempt to parse a value of type @r@ from a list of arguments,
-- where the parser @ParseTree s r@ doesn't support requests.
runSilentParser
  :: (Scheme s, RequestSupport s ~ 'False)
  => ParseTree s r -- ^ Argument parser
  -> [Text] -- ^ Input arguments
  -> Result s r
runSilentParser tree = runSilentParser' tree . argsToState

-- | A more general form of 'runSilentParser' that accepts a custom
-- stream starting state.
runSilentParser'
  :: (Scheme s, RequestSupport s ~ 'False)
  => ParseTree s r -- ^ Argument parser
  -> StreamState s -- ^ Initial stream state
  -> Result s r
runSilentParser' tree state =
  runArgumentParser' tree state Success Failure NoRequests

-- | Attempt to parse a value of type @r@ from a list of arguments,
-- where the parser @ParseTree s r@ supports requests.
runHelpfulParser
  :: SupportsResponse s
  => ProgramInfo s -- ^ Program metadata
  -> ParseTree s r -- ^ Argument parser
  -> [Text] -- ^ Input arguments
  -> Result s r
runHelpfulParser info tree = runHelpfulParser' info tree . argsToState

-- | A more general form of 'runHelpfulParser' that accepts a custom
-- stream starting state.
runHelpfulParser'
  :: SupportsResponse s
  => ProgramInfo s -- ^ Program metadata
  -> ParseTree s r -- ^ Argument parser
  -> StreamState s -- ^ Initial stream state
  -> Result s r
runHelpfulParser' info tree state =
  runArgumentParser' tree state Success Failure (OnRequest _onRequest)
  where
    _onRequest state' HelpRequest =
      Response $ makeHelpInfo tree (streamContext state') info
    _onRequest _ VersionRequest =
      Response $ makeVersionInfo info

-- | A variant of 'runHelpfulParser' that treats requests as failures.
--
-- This is useful if you know that no requests will ever be made.
runHelpfulParser_
  :: SupportsResponse s
  => ParseTree s r -- ^ Argument parser
  -> [Text] -- ^ Input arguments
  -> Result s r
runHelpfulParser_ tree args =
  runArgumentParser' tree (argsToState args) Success Failure (OnRequest _onRequest)
  where
    _onRequest state' _ = Failure $
      formatError (streamContext state') "help requested"

-- | Parse the command line arguments passed to the program, then
-- invoke the program's entrypoint with the results of the parsing. If
-- parsing fails, we instead display an error to stderr and exit.
-- Alternatively, if information was requested, we abandon parsing and
-- print the relevant response to stdout, then exit without indicating
-- an error.
parseArguments
  :: SupportsResponse s
  => ProgramInfo s -- ^ Program metadata
  -> ParseTree s r -- ^ Argument parser
  -> (r -> IO a) -- ^ Program Entrypoint
  -> IO a
parseArguments info tree action = do
  args <- map T.pack <$> getArgs
  case runHelpfulParser info tree args of
    Success [] result -> action result
    Success (token:_) _ -> do
      hPutBuilder stderr $ "unexpected " <> render token <> "\n"
      exitFailure
    Failure err -> do
      TIO.hPutStrLn stderr err
      exitFailure
    Response output -> do
      TIO.putStr output
      exitSuccess

-- | Satiate a 'ParseTree' with all the input it can consume, then
-- attempt to evaluate it.
runArgumentParser
  :: Scheme s
  => ParseTree s r -- ^ Argument parser
  -> [Text] -- ^ Input arguments
  -> ([Text] -> r -> a) -- ^ Success handler
  -> (Text -> a) -- ^ Failure handler
  -> RequestHandler s a -- ^ Request handler
  -> a
runArgumentParser tree = runArgumentParser' tree . argsToState

-- | A more general form of 'runArgumentParser' that accepts a custom
-- stream starting state.
runArgumentParser'
  :: Scheme s
  => ParseTree s r -- ^ Argument parser
  -> StreamState s -- ^ Initial stream state
  -> ([Text] -> r -> a) -- ^ Success handler
  -> (Text -> a) -- ^ Failure handler
  -> RequestHandler s a -- ^ Request handler
  -> a
runArgumentParser' tree state cok cerr hhelp =
  runStreamParser (satiate tree) handler state
  where
    _onFailure state' = cerr . formatError (streamContext state')
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
      , onRequest = hhelp
      }
