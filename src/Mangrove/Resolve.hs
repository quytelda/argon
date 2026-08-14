{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mangrove.Resolve
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Resolvable parsers represent expressions that can be evaluated to a
value once they have received the appropriate input.
-}
module Mangrove.Resolve
  ( -- * Resolution
    Resolve(..)
  , ResolveM(..)
  , renderExpectedError
  , resolveLifted
  ) where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.List            as List
import           Mangrove.Text

-- | A monad for resolving parsers and expression trees.
--
-- We track two kinds of failure: (1) we expected something specific
-- and didn't find it, and (2) the parser resolved to an empty value.
data ResolveM a
  = EmptyError
  | ExpectedError ![Builder]
  | Value !a
  deriving (Functor)

instance Applicative ResolveM where
  pure = Value

  Value f <*> r          = fmap f r
  ExpectedError es <*> _ = ExpectedError es
  EmptyError <*> _       = EmptyError

instance Alternative ResolveM where
  empty = EmptyError

  ExpectedError es1 <|> ExpectedError es2 = ExpectedError $ es1 <> es2
  l@(Value _) <|> _                       = l
  _ <|> r                                 = r

instance Monad ResolveM where
  return = pure

  Value a >>= f          = f a
  ExpectedError es >>= _ = ExpectedError es
  EmptyError >>= _       = EmptyError

-- | An error message for 'EmptyError's.
renderEmptyError :: Builder
renderEmptyError = "empty"

-- | Format an error message for 'ExpectedError's.
renderExpectedError :: [Builder] -> Builder
renderExpectedError es =
  "expected: " <> (mconcat . List.intersperse " or ") es

-- | Things that can be resolved to a value, but might fail to
-- resolve.
class Resolve m where
  resolve :: m r -> ResolveM r

-- | Lift 'resolve' into 'MonadError Builder'.
resolveLifted :: (Resolve f, MonadError Builder m) => f r -> m r
resolveLifted mr = case resolve mr of
  EmptyError       -> throwError renderEmptyError
  ExpectedError es -> throwError $ renderExpectedError es
  Value a          -> pure a
