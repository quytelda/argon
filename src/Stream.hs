{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Stream
Copyright   : (c) Quytelda Kahja, 2025
License     : BSD-3-Clause

Provides a basic stream-parsing monad for parsing a token sequences
with error handling and context management.
-}
module Stream
  ( -- * Types
    ParseResult(..)
  , StreamParser(..)

    -- * Context
  , errorInContext
  , pushContext
  , popContext
  , getContexts
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

-- | The result of a StreamParser computation.
data ParseResult tok a
  = ParseResult [Builder] [tok] a -- ^ A successful result
  | ParseEmpty [Builder] [tok] -- ^ The parser doesn't apply
  | ParseError [Builder] Builder -- ^ The parser failed
  deriving (Functor)

instance Semigroup (ParseResult tok a) where
  l <> r =
    case (l, r) of
      (ParseError {}, _)  -> l
      (_, ParseError {})  -> r
      (ParseEmpty {}, _)  -> r
      (ParseResult {}, _) -> l

-- | The amazing stream parsing monad! This monad is comparable to a
-- combination of StateT, ExceptT, and MaybeT. It tracks state (the
-- current context and the stream state) and handles pure exceptions.
newtype StreamParser tok a = StreamParser
  { runStreamParser :: [Builder] -> [tok] -> ParseResult tok a }
  deriving (Functor)

instance Applicative (StreamParser tok) where
  pure a = StreamParser $ \cs ts -> ParseResult cs ts a
  mf <*> ma = StreamParser $ \cs ts ->
    case runStreamParser mf cs ts of
      ParseResult cs' ts' f -> f <$> runStreamParser ma cs' ts'
      ParseEmpty cs' ts'    -> ParseEmpty cs' ts'
      ParseError cs' err    -> ParseError cs' err

instance Alternative (StreamParser tok) where
  empty = StreamParser $ \s -> ParseEmpty s
  l <|> r = StreamParser $ \s ->
    runStreamParser l s <> runStreamParser r s

instance Monad (StreamParser tok) where
  return = pure
  mf >>= f = StreamParser $ \cs ts ->
    case runStreamParser mf cs ts of
      ParseResult cs' ts' a -> runStreamParser (f a) cs' ts'
      ParseEmpty cs' ts'    -> ParseEmpty cs' ts'
      ParseError cs' e      -> ParseError cs' e

instance MonadError Builder (StreamParser tok) where
  throwError err = StreamParser $ \cs _ -> ParseError cs err
  catchError ma handler = StreamParser $ \cs ts ->
    case runStreamParser ma cs ts of
      ParseError cs' err -> runStreamParser (handler err) cs' ts
      result             -> result

--------------------------------------------------------------------------------

-- | Format an error message prepended with context information.
errorInContext :: [Builder] -> Builder -> Builder
errorInContext contexts =
  mconcat
  . List.reverse
  . List.intersperse ": "
  . (: contexts)

-- | Access the context stack.
getContexts :: StreamParser tok [Builder]
getContexts = StreamParser $ \cs ts -> ParseResult cs ts cs

-- | Push a new context to the context stack.
pushContext :: Builder -> StreamParser tok ()
pushContext context = StreamParser $ \cs ts -> ParseResult (context : cs) ts ()

-- | Pop the top context from the stack, if there is one.
popContext :: StreamParser tok (Maybe Builder)
popContext = StreamParser $ \cs ts ->
  case cs of
    (c : cs') -> ParseResult cs' ts $ Just c
    _         -> ParseResult cs ts Nothing

-- | Push the given context onto the stack, perform a computation,
-- then pop it off. This assumes the computation doesn't modify the
-- stack.
withContext :: Builder -> StreamParser tok a -> StreamParser tok a
withContext context action = pushContext context *> action <* popContext

--------------------------------------------------------------------------------

-- | Remove and return the first token in the stream.
popMaybe :: StreamParser tok (Maybe tok)
popMaybe = StreamParser $ \cs ts ->
  case ts of
    (t:ts') -> ParseResult cs ts' $ Just t
    _       -> ParseResult cs ts Nothing

-- | View the first token in the stream without consuming it.
peekMaybe :: StreamParser tok (Maybe tok)
peekMaybe = StreamParser $ \cs ts ->
  case ts of
    (t:_) -> ParseResult cs ts $ Just t
    _     -> ParseResult cs ts Nothing

-- | Remove and return the first token in the stream. Evaluates to
-- 'empty' if there are no tokens in the stream.
pop :: StreamParser tok tok
pop = StreamParser $ \cs ts ->
  case ts of
    (t:ts') -> ParseResult cs ts' t
    _       -> ParseEmpty cs ts

-- | View the first token in the stream without consuming it.
-- Evaluates to 'empty' if there are no tokens in the stream.
peek :: StreamParser tok tok
peek = StreamParser $ \cs ts ->
  case ts of
    (t:_) -> ParseResult cs ts t
    _     -> ParseEmpty cs ts

-- | Prepend a token to the front of the stream.
push :: tok -> StreamParser tok ()
push t = StreamParser $ \cs ts -> ParseResult cs (t:ts) ()

-- | Pop the first token and discard it. Nothing happens if there are
-- no tokens in the stream.
pop_ :: StreamParser tok ()
pop_ = StreamParser $ \cs ts -> ParseResult cs (drop 1 ts) ()
