{-|
Module      : Mangrove
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

This module contains types and functions necessary for running
argument parsers.
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
