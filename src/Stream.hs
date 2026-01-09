{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolymorphicComponents     #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : Stream
Copyright   : (c) Quytelda Kahja, 2025
License     : BSD-3-Clause

Provides a basic stream-parsing monad for parsing a token sequences
with error handling and context management.
-}
module Stream
  ( -- * Types
    StreamParser(..)

    -- * Context
  , withContext

    -- * Stream
  , popMaybe
  , peekMaybe
  , pop
  , peek
  , push
  , pop_
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Text.Lazy.Builder (Builder)

-- | The amazing stream parsing monad! This monad is comparable to a
-- combination of StateT, ExceptT, and MaybeT. It tracks the stream
-- state and handles pure exceptions.
newtype StreamParser tok a = StreamParser
  { runStreamParser
    :: forall r.
       (a -> [tok] -> r) -- success continuation
    -> ([tok] -> r) -- empty continuation
    -> (Builder -> r) -- error continuation
    -> [tok] -- stream content
    -> r
  }

instance Functor (StreamParser tok) where
  fmap f parser = StreamParser $ \cok ->
    runStreamParser parser (cok . f)

instance Applicative (StreamParser tok) where
  pure a = StreamParser $ \cok _ _ -> cok a
  mf <*> ma = StreamParser $ \cok cempty cerr ->
    runStreamParser mf
    (\f -> runStreamParser ma (cok . f) cempty cerr)
    cempty
    cerr

instance Alternative (StreamParser tok) where
  empty = StreamParser $ \_ cempty _ -> cempty
  l <|> r = StreamParser $ \cok cempty cerr ->
    runStreamParser l cok
    (runStreamParser r cok cempty cerr)
    cerr

instance Monad (StreamParser tok) where
  return = pure
  ma >>= f = StreamParser $ \cok cempty cerr ->
    runStreamParser ma
    (\a -> runStreamParser (f a) cok cempty cerr)
    cempty
    cerr

instance MonadError Builder (StreamParser tok) where
  throwError err = StreamParser $ \_ _ cerr _ -> cerr err
  catchError ma handler = StreamParser $ \cok cempty cerr ts ->
    runStreamParser ma cok cempty
    (\err -> runStreamParser (handler err) cok cempty cerr ts)
    ts

--------------------------------------------------------------------------------

-- | Push the given context onto the stack, perform a computation,
-- then pop it off. This assumes the computation doesn't modify the
-- stack.
withContext :: Builder -> StreamParser tok a -> StreamParser tok a
withContext context action = StreamParser $ \cok cempty cerr ->
  runStreamParser action cok cempty (cerr . prepend context)
  where
    prepend s1 s2 = s1 <> ": " <> s2

--------------------------------------------------------------------------------

-- | Remove and return the first token in the stream.
popMaybe :: StreamParser tok (Maybe tok)
popMaybe = StreamParser $ \cok _ _ ts ->
  case ts of
    (t:ts') -> cok (Just t) ts'
    _       -> cok Nothing ts

-- | View the first token in the stream without consuming it.
peekMaybe :: StreamParser tok (Maybe tok)
peekMaybe = StreamParser $ \cok _ _ ts ->
  case ts of
    (t:_) -> cok (Just t) ts
    _     -> cok Nothing ts

-- | Remove and return the first token in the stream. Evaluates to
-- 'empty' if there are no tokens in the stream.
pop :: StreamParser tok tok
pop = StreamParser $ \cok cempty _ ts ->
  case ts of
    (t:ts') -> cok t ts'
    _       -> cempty ts

-- | View the first token in the stream without consuming it.
-- Evaluates to 'empty' if there are no tokens in the stream.
peek :: StreamParser tok tok
peek = StreamParser $ \cok cempty _ ts ->
  case ts of
    (t:_) -> cok t ts
    _     -> cempty ts

-- | Prepend a token to the front of the stream.
push :: tok -> StreamParser tok ()
push t = StreamParser $ \cok _ _ -> cok () . (t:)

-- | Discard the first token in the stream. Nothing happens if there
-- are no tokens in the stream.
pop_ :: StreamParser tok ()
pop_ = StreamParser $ \cok _ _ -> cok () . drop 1
