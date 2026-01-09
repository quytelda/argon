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
  , errorInContext
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
import qualified Data.List              as List
import           Data.Text.Lazy.Builder (Builder)

-- | The amazing stream parsing monad! This monad is comparable to a
-- combination of StateT, ExceptT, and MaybeT. It tracks state (the
-- current context and the stream state) and handles pure exceptions.
newtype StreamParser tok a = StreamParser
  { runStreamParser
    :: forall r.
       [tok]
    -> (a -> [tok] -> r) -- success
    -> ([tok] -> r) -- empty
    -> (Builder -> r) -- error
    -> r
  }

instance Functor (StreamParser tok) where
  fmap f parser = StreamParser $ \ts cok cempty cerr ->
    runStreamParser parser ts (cok . f) cempty cerr

instance Applicative (StreamParser tok) where
  pure a = StreamParser $ \ts cok _ _ -> cok a ts
  mf <*> ma = StreamParser $ \ts cok cempty cerr ->
    runStreamParser mf ts (\f ts' ->
                                runStreamParser ma ts' (cok . f) cempty cerr
                             ) cempty cerr

instance Alternative (StreamParser tok) where
  empty = StreamParser $ \ts _ cempty _ -> cempty ts
  l <|> r = StreamParser $ \ts cok cempty cerr ->
    runStreamParser l ts cok (\ts' -> runStreamParser r ts' cok cempty cerr) cerr

instance Monad (StreamParser tok) where
  return = pure
  ma >>= f = StreamParser $ \ts cok cempty cerr ->
    runStreamParser ma ts (\a ts' ->
                                runStreamParser (f a) ts' cok cempty cerr
                          ) cempty cerr

instance MonadError Builder (StreamParser tok) where
  throwError err = StreamParser $ \_ _ _ cerr -> cerr err
  catchError ma handler = StreamParser $ \ts cok cempty cerr ->
    runStreamParser ma ts cok cempty (\err -> runStreamParser (handler err) ts cok cempty cerr)

--------------------------------------------------------------------------------

-- | Format an error message prepended with context information.
errorInContext :: [Builder] -> Builder -> Builder
errorInContext contexts =
  mconcat
  . List.reverse
  . List.intersperse ": "
  . (: contexts)

-- | Access the context stack.
-- getContexts :: StreamParser tok [Builder]
-- getContexts = StreamParser $ \cs ts cok _ _ -> cok cs cs ts

-- | Push a new context to the context stack.
-- pushContext :: Builder -> StreamParser tok ()
-- pushContext context = StreamParser $ \cs ts cok _ _ -> cok () (context : cs) ts

-- | Pop the top context from the stack, if there is one.
-- popContext :: StreamParser tok (Maybe Builder)
-- popContext = StreamParser $ \cs ts cok _ _ ->
--   case cs of
--     (c : cs') -> cok (Just c) cs' ts
--     _         -> cok Nothing cs ts

-- | Push the given context onto the stack, perform a computation,
-- then pop it off. This assumes the computation doesn't modify the
-- stack.
withContext :: Builder -> StreamParser tok a -> StreamParser tok a
withContext context action = StreamParser $ \ts cok cempty cerr ->
  runStreamParser action ts cok cempty (cerr . prepend context)
  where
    prepend s1 s2 = s1 <> ": " <> s2

--------------------------------------------------------------------------------

-- | Remove and return the first token in the stream.
popMaybe :: StreamParser tok (Maybe tok)
popMaybe = StreamParser $ \ts cok _ _ ->
  case ts of
    (t:ts') -> cok (Just t) ts'
    _       -> cok Nothing ts

-- | View the first token in the stream without consuming it.
peekMaybe :: StreamParser tok (Maybe tok)
peekMaybe = StreamParser $ \ts cok _ _->
  case ts of
    (t:_) -> cok (Just t) ts
    _     -> cok Nothing ts

-- | Remove and return the first token in the stream. Evaluates to
-- 'empty' if there are no tokens in the stream.
pop :: StreamParser tok tok
pop = StreamParser $ \ts cok cempty _ ->
  case ts of
    (t:ts') -> cok t ts'
    _       -> cempty ts

-- | View the first token in the stream without consuming it.
-- Evaluates to 'empty' if there are no tokens in the stream.
peek :: StreamParser tok tok
peek = StreamParser $ \ts cok cempty _ ->
  case ts of
    (t:_) -> cok t ts
    _     -> cempty ts

-- | Prepend a token to the front of the stream.
push :: tok -> StreamParser tok ()
push t = StreamParser $ \ts cok _ _ -> cok () (t:ts)

-- | Pop the first token and discard it. Nothing happens if there are
-- no tokens in the stream.
pop_ :: StreamParser tok ()
pop_ = StreamParser $ \ts cok _ _ -> cok () (drop 1 ts)
