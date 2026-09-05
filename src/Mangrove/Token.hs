{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Mangrove.Token
  ( HasTokens(..)
  ) where

import           Data.Kind
import           Data.Proxy

import           Mangrove.Text

class (Eq (Token s), Render (Token s), Show (Token s)) => HasTokens (s :: Type -> Type) where
  -- | A token represents a particular interpretation of an argument
  -- string.
  data Token s

  -- | 'delimiter' is the character that separates argument strings in
  -- combined string representation. For example, arguments in the CLI
  -- command @ls -a -l /var@ are separated by spaces.
  delimiter :: Proxy s -> Char

  -- | This type indicates whether a parsing scheme accepts requests
  -- for information.
  --
  -- When @RequestSupport scheme@ is @True@, a 'SupportsResponse'
  -- instance should be provided for @scheme@.
  type RequestSupport s :: Bool
  type RequestSupport s = 'False
