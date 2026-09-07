{-|
Module      : Mangrove
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

This module exports the full API required for running any generic
parser. For constructing parsers, you'll need to import the building
blocks for the specific kind of parser you are building. For example,
"Mangrove.Unix" contains the tools for building UNIX-style parsers.
-}
module Mangrove
  ( module Mangrove.Parser

  -- * Re-exported Types
  , ParseTree
  , Scheme
  , ProgramInfo(..)
  , SupportsResponse
  , StreamState
  , RequestType(..)
  , RequestHandler
  , ReqContinuation(..)
  ) where

import           Mangrove.Parser
import           Mangrove.ParseTree
import           Mangrove.Scheme
import           Mangrove.Stream
