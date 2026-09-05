{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolymorphicComponents     #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

{-|
Module      : Mangrove.Stream
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Provides a basic stream-parsing monad for parsing argument sequences
with error handling and context management.
-}

module Mangrove.Stream
  ( -- * Stream Parser
    ParserInfo(..)
  , StreamParser(..)
  , StreamHandler(..)
  , StreamState(..)
  , RequestHandler
  , ReqContinuation(..)

    -- ** Requests
  , RequestType(..)
  , request

    -- ** Escaping
  , setEscaped
  , getEscaped

    -- ** Context
  , getContext
  , setContext
  , withContext
  , formatError

    -- ** Streaming
  , popMaybe
  , peekMaybe
  , pop
  , peek
  , push
  , pop_
) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Kind
import qualified Data.List              as List
import           Data.Proxy
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

import           Mangrove.Text

-- | A class for types that have all the properties needed for parsing
-- with the 'StreamParser' monad.
class (Eq (Token s), Render (Token s), Show (Token s)) => ParserInfo (s :: Type -> Type) where
  -- | A token represents a particular interpretation of an argument
  -- string.
  data Token s

  -- | This type indicates whether a parsing scheme accepts requests
  -- for information.
  --
  -- When @RequestSupport scheme@ is @True@, a
  -- 'Mangrove.Scheme.SupportsResponse' instance should be provided
  -- for @scheme@.
  type RequestSupport s :: Bool
  type RequestSupport s = 'False

  -- | 'delimiter' is the character that separates argument strings in
  -- combined string representation. For example, arguments in the CLI
  -- command @ls -a -l /var@ are separated by spaces.
  delimiter :: Proxy s -> Char

-- | The current state of a stream parser.
--
-- The content of a stream is just a list of 'Text' values. The
-- context stack is a list of tokens currently being processed; when a
-- token is recognized, it gets added to front of the list while the
-- token is being parsed into a usable value. When this parsing
-- completes, the token is popped from the front of the list.
--
-- A streams can also enable "escaped" mode by setting 'streamEscaped'
-- to 'True'. What this actually does is parser-dependant, but usually
-- it restricts how subsequent arguments can be interpreted. For
-- example, in the Unix scheme, escaping forces all subsequent
-- arguments to be interpreted as positional arguments, even if they
-- would normally be interpreted as options or commands.
data StreamState s = StreamState
  { streamContent :: ![Text]    -- ^ A sequence of 'Text' values
  , streamContext :: ![Token s] -- ^ A stack representing current parsing context
  , streamEscaped :: !Bool      -- ^ Escaped mode
  }

deriving instance ParserInfo s => Show (StreamState s)
deriving instance ParserInfo s => Eq (StreamState s)

-- | What information is being requested?
data RequestType
  = VersionRequest -- ^ A request for version information
  | HelpRequest -- ^ A request for help and usage information
  deriving (Eq, Show)

-- | A handler for when information is requested.
--
-- This will hold a continuation function for helpful parsing
-- schemes, or a placeholder value for silent schemes.
data family ReqContinuation (cap :: Bool) (s :: Type -> Type) r

data instance ReqContinuation 'False s r
  = NoRequests
  deriving (Functor)

newtype instance ReqContinuation 'True s r
  = OnRequest (StreamState s -> RequestType -> r)
  deriving (Functor)

-- | A handler for when information is requested.
--
-- This will hold a continuation function for helpful parsing
-- schemes, or a placeholder value for silent schemes.
type RequestHandler s r = ReqContinuation (RequestSupport s) s r

-- | A collection of continuations to be called for each situation a
-- stream parser might encounter.
data StreamHandler s a r = StreamHandler
  { onSuccess :: StreamState s -> a -> r -- ^ Success Continuation
  , onEmpty   :: StreamState s -> r -- ^ Empty continuation
  , onFailure :: StreamState s -> Builder -> r -- ^ Failure Continuation
  , onRequest :: RequestHandler s r -- ^ Request Continuation
  }

-- | The amazing stream parsing monad! This monad tracks the stream
-- state and context. It short-circuits when exceptions or requests
-- are raised.
newtype StreamParser s a = StreamParser
  { runStreamParser
    :: forall r. StreamHandler s a r
    -> StreamState s
    -> r
  }

instance Functor (StreamParser s) where
  fmap f parser = StreamParser $ \handler ->
    runStreamParser parser handler { onSuccess = \s -> onSuccess handler s . f }

instance Applicative (StreamParser s) where
  pure a = StreamParser $ \handler state -> onSuccess handler state a
  mf <*> ma = StreamParser $ \handler ->
    runStreamParser mf
    handler { onSuccess = \s f -> runStreamParser ma handler { onSuccess = \s' -> onSuccess handler s' . f } s }

instance Alternative (StreamParser s) where
  empty = StreamParser $ \handler -> onEmpty handler
  l <|> r = StreamParser $ \handler ->
    runStreamParser l handler { onEmpty = runStreamParser r handler }

instance Monad (StreamParser s) where
  return = pure
  ma >>= f = StreamParser $ \handler ->
    runStreamParser ma handler { onSuccess = \s a -> runStreamParser (f a) handler s }

instance MonadError Builder (StreamParser s) where
  throwError err = StreamParser $ \handler state -> onFailure handler state err
  catchError ma recover = StreamParser $ \handler state ->
    runStreamParser ma
    handler { onFailure = \_ err -> runStreamParser (recover err) handler state }
    state

-- | Enable or disable escaped parsing. What this actually does is
-- parser-dependant, but usually it restricts how subsequent arguments
-- can be interpreted. For example, in the Unix scheme, escaping
-- forces all subsequent arguments to be interpreted as positional
-- arguments, even if they would normally be interpreted as options or
-- commands.
setEscaped :: Bool -> StreamParser s ()
setEscaped b = StreamParser $ \handler state ->
  onSuccess handler state { streamEscaped = b } ()

-- | Check whether escaped parsing is enabled.
getEscaped :: StreamParser s Bool
getEscaped = StreamParser $ \handler state ->
  onSuccess handler state (streamEscaped state)

-- | Signal that information is requested. Short-circuits any further
-- operations.
request :: RequestSupport s ~ 'True => RequestType -> StreamParser s a
request requestType = StreamParser $ \handler state ->
  case onRequest handler of
    OnRequest h -> h state requestType

-- | Get a list representing the current context stack.
getContext :: StreamParser s [Token s]
getContext = StreamParser $ \handler state ->
  onSuccess handler state (streamContext state)

-- | Replace the context stack.
setContext :: [Token s] -> StreamParser s ()
setContext contexts = StreamParser $ \handler state ->
  onSuccess handler state { streamContext = contexts } ()

-- | Push the provided token onto the context stack, then perform some
-- computation. Afterwards, the stack is restored to its prior state.
withContext :: Token s -> StreamParser s a -> StreamParser s a
withContext context action = do
  oldContext <- getContext
  setContext $ context : oldContext
  action <* setContext oldContext

-- | Format an error message with context information.
formatError :: Render tok => [tok] -> Builder -> Text
formatError contexts err =
  TL.toStrict
  $ TLB.toLazyText
  $ mconcat
  $ List.intersperse ": "
  $ reverse
  $ err : map render contexts

--------------------------------------------------------------------------------

-- | Remove and return the first token in the stream.
popMaybe :: StreamParser s (Maybe Text)
popMaybe = StreamParser $ \handler state ->
  case streamContent state of
    (t:ts') -> onSuccess handler state { streamContent = ts' } (Just t)
    _       -> onSuccess handler state Nothing

-- | View the first token in the stream without consuming it.
peekMaybe :: StreamParser s (Maybe Text)
peekMaybe = StreamParser $ \handler state ->
  case streamContent state of
    (t:_) -> onSuccess handler state (Just t)
    _     -> onSuccess handler state Nothing

-- | Remove and return the first token in the stream. Evaluates to
-- 'empty' if there are no tokens in the stream.
pop :: StreamParser s Text
pop = StreamParser $ \handler state ->
  case streamContent state of
    (t:ts') -> onSuccess handler state { streamContent = ts' } t
    _       -> onEmpty handler state

-- | View the first token in the stream without consuming it.
-- Evaluates to 'empty' if there are no tokens in the stream.
peek :: StreamParser s Text
peek = StreamParser $ \handler state ->
  case streamContent state of
    (t:_) -> onSuccess handler state t
    _     -> onEmpty handler state

-- | Prepend a token to the front of the stream.
push :: Text -> StreamParser s ()
push t = StreamParser $ \handler state ->
  onSuccess handler
  state { streamContent = t : streamContent state }
  ()

-- | Discard the first token in the stream. Nothing happens if there
-- are no tokens in the stream.
pop_ :: StreamParser s ()
pop_ = StreamParser $ \handler state ->
  onSuccess handler
  state { streamContent = drop 1 $ streamContent state }
  ()
