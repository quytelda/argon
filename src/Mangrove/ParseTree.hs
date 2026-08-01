{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolymorphicComponents     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : Mangrove.ParseTree
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

This module contains the data types and type classes that make up a
generic argument parser, as well as a stream parsing monad in which
parsing takes place.

A 'ParseTree' is a tree-shaped parser that "filter-feeds" on a stream
of arguments, collecting inputs at the leaves and feeding the results
up the tree for processing. 'ParseTree's are parameterized by the
parser scheme that determines the kind of inputs it accepts.

A "scheme" is a system of parsers and tokens. It determines the method
by which argument strings are separated. It parses a sequence of
arguments into tokens and values.
-}
module Mangrove.ParseTree
  ( -- * Parse Trees
    ParseTree(..)
  , isProduct
  , isSum
  , isOptional
  , isChoice

    -- * Parsing Schemes
  , Scheme(..)

    -- * Stream Parser
  , StreamParser(..)
  , StreamHandler(..)
  , StreamState(..)

    -- * Monadic Actions
    -- ** Help
  , requestHelp

    -- ** Escaping
  , setEscaped
  , getEscaped

    -- ** Context
  , getContext
  , setContext
  , withContext
  , renderError

    -- ** Streaming
  , popMaybe
  , peekMaybe
  , pop
  , peek
  , push
  , pop_

    -- * Feeding Trees
  , satiate
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Kind
import qualified Data.List            as List
import           Data.Maybe
import           Data.Proxy
import           Data.Text            (Text)

import           Mangrove.Resolve
import           Mangrove.Separable
import           Mangrove.Text
import           Mangrove.Valency

--------------------------------------------------------------------------------
-- Parse Trees

-- | `ParseTree scheme r` is an expression tree composed of parsers
-- from scheme @scheme@ which evaluates to a value of type @r@ when
-- supplied with the proper input.
data ParseTree (scheme :: Type -> Type) (r :: Type) where
  -- | Terminal node with no value (abstracts 'empty')
  EmptyNode :: ParseTree scheme r
  -- | A terminal node with a resolved value (abstracts 'pure')
  ValueNode :: r -> ParseTree scheme r
  -- | A parser awaiting input
  ParseNode :: scheme r -> ParseTree scheme r
  -- | Abstracts 'liftA2' and by extension '(<*>)'
  ProdNode :: (u -> v -> r) -> ParseTree scheme u -> ParseTree scheme v -> ParseTree scheme r
  -- | Abstracts '(<|>)'
  SumNode :: ParseTree scheme r -> ParseTree scheme r -> ParseTree scheme r
  -- | Abstracts 'many' (@MaybeNode False@) and 'some' (@MaybeNode True@)
  ManyNode :: Bool -> ParseTree scheme r -> ParseTree scheme [r]

instance Functor p => Functor (ParseTree p) where
  fmap _ EmptyNode          = EmptyNode
  fmap f (ValueNode value)  = ValueNode $ f value
  fmap f (ParseNode parser) = ParseNode $ fmap f parser
  fmap f (ProdNode g l r)   = ProdNode (\u v -> f $ g u v) l r
  fmap f (SumNode l r)      = SumNode (fmap f l) (fmap f r)
  fmap f node               = liftA2 ($) (pure f) node
  -- This takes advantage of the fact that f <$> x = pure f <*> x.

instance Functor p => Applicative (ParseTree p) where
  pure = ValueNode
  liftA2 = ProdNode

instance Functor p => Alternative (ParseTree p) where
  empty = EmptyNode
  (<|>) = SumNode
  many = ManyNode False
  some = ManyNode True

instance Valency s => Valency (ParseTree s) where
  valency EmptyNode         = Just 0
  valency (ValueNode _)     = Just 0
  valency (ParseNode p)     = valency p
  valency (ProdNode _ l r)  = (+) <$> valency l <*> valency r
  valency (SumNode l r)     = max <$> valency l <*> valency r
  valency (ManyNode _ tree) =
    case valency tree of
      Just n | n <= 0 -> Just 0
      _               -> Nothing -- i.e. infinity
  -- In the above case of 'ManyNode _ p', a ManyNode can accept an
  -- arbitrary number of parameters, so the maximum valency is either
  -- infinite or zero depending on whether the valency of 'p' is zero.

  -- Since ParseTrees themselves don't accept inputs, we can provide a
  -- slightly more efficient implementation of nullary.
  nullary EmptyNode         = True
  nullary (ValueNode _)     = True
  nullary (ParseNode p)     = nullary p
  nullary (ProdNode _ l r)  = nullary l && nullary r
  nullary (SumNode l r)     = nullary l && nullary r
  nullary (ManyNode _ tree) = nullary tree

instance Resolve s => Resolve (ParseTree s) where
  resolve EmptyNode          = EmptyError
  resolve (ValueNode value)  = pure value
  resolve (ParseNode parser) = resolve parser
  resolve (ProdNode f l r)   = f <$> resolve l <*> resolve r
  resolve (SumNode l r)      = resolve l <|> resolve r
  resolve (ManyNode False _) = pure []
  resolve (ManyNode True  p) = pure <$> resolve p
  -- NOTE: If a ManyNode contains a resolvable node, one might expect
  -- the result to be an infinite list (e.g. `resolve $ many
  -- (ValueNode 1)` to give `Right [1,1,1,1,..]`) or for the
  -- computation to diverge (as is the case for `many (Just 1)`).
  -- However, by only attempting at most resolutions of the subtree,
  -- we will get either zero or one results. For example, `resolve $
  -- many (ValueNode 1)` will give `Right []`.
  --
  -- Whether this is the best possible way to handle the situation is
  -- unclear. This avoids infinite loops, but might not be the
  -- expected behavior in some unforseen use-case.

-- | Is this a 'ProdNode'?
isProduct :: ParseTree s r -> Bool
isProduct (ProdNode {}) = True
isProduct _             = False

-- | Is this a 'SumNode'?
isSum :: ParseTree s r -> Bool
isSum (SumNode {}) = True
isSum _            = False

-- | Does this subtree accept optional input?
isOptional :: ParseTree s r -> Bool
isOptional (SumNode _ (ValueNode {})) = True
isOptional (ManyNode False _)         = True
isOptional _                          = False

-- | Is this a 'SumNode' that does *not* represent an optional input.
isChoice :: ParseTree s r -> Bool
isChoice = liftA2 (&&) isSum (not . isOptional)

instance (Valency s, Scheme s) => Render (ParseTree s r) where
  -- special cases
  render (SumNode p (ValueNode _)) = brackets $ render p

  render (ParseNode parser) = usageInfo parser
  render (ProdNode _ l r)
    | nullary l && nullary r = ""
    | nullary l = _render r
    | nullary r = _render l
    | otherwise = _render l <> render sep <> _render r
    where
      _render = renderDelimitedIf braces isChoice
      sep = delimiter (Proxy @s)
  render (SumNode l r)
    | nullary l && nullary r = ""
    | nullary l = _render r
    | nullary r = _render l
    | otherwise = _render l <> "|" <> _render r
    where
      _render = renderDelimitedIf braces isProduct
  render (ManyNode required p) = wrap $ render p <> "..."
    where
      wrap = if required
             then braces
             else brackets

  -- Constant nodes that don't accept input have no usage.
  render _ = ""

instance Separable s => Separable (ParseTree s) where
  separate (SumNode l r) = Exhibit norm (modalsL <> modalsR)
    where
      Exhibit normL modalsL = separate l
      Exhibit normR modalsR = separate r
      norm = liftA2 SumNode normL normR
             <|> normL
             <|> normR
  separate (ProdNode f l r) = Exhibit norm modals
    where
      Exhibit normL modalsL = separate l
      Exhibit normR modalsR = separate r
      node = ProdNode f
      norm = liftA2 node normL normR
      cross g modalTrees normalTrees =
        [ g (if usesTerseOutput m && isOptional n then empty else n) <$> m
        | m <- modalTrees
        , n <- normalTrees
        ]
      modals = cross (flip node) modalsL (maybeToList normR)
               <> cross node modalsR (maybeToList normL)
               <> [liftA2 node u v | u <- modalsL, v <- modalsR]
  separate (ParseNode p) = ParseNode <$> separate p
  separate n = Exhibit (Just n) []

--------------------------------------------------------------------------------
-- Parsing Schemes

-- | A scheme is a system of parsers and tokens. It parses a sequence
-- of arguments into tokens and values.
class (Functor s, Resolve s) => Scheme (s :: Type -> Type) where
  -- | A token represents a particular interpretation of an argument
  -- string under this parsing scheme.
  data Token s

  -- | 'delimiter' is the character that separates argument strings in
  -- combined string representation. For example, arguments in the CLI
  -- command @ls -a -l /var@ are separated by spaces.
  delimiter :: Proxy s -> Char

  -- | Parse special control arguments that don't represent tokens in
  -- the scheme, but control aspects of how parsing proceeds (e.g.
  -- escaping).
  parseSpecials :: StreamParser (Token s) ()
  parseSpecials = pure ()

  -- | 'activate' tries to run a parser on the current input. If the
  -- parser doesn't apply, it consumes nothing and returns empty. If
  -- it does apply, it consumes the relevant input and returns a
  -- result.
  activate :: s r -> StreamParser (Token s) r

  -- | Render human-readable usage information for a particular
  -- parser.
  usageInfo :: s r -> Builder

--------------------------------------------------------------------------------
-- Stream Parser

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
data StreamState tok = StreamState
  { streamContent :: [Text] -- ^ A sequence of 'Text' values
  , streamContext :: [tok]  -- ^ A stack representing current parsing context
  , streamEscaped :: Bool   -- ^ Escaped mode
  } deriving (Eq, Show)

-- | A collection of continuations to be called for each situation a
-- stream parser might encounter.
data StreamHandler tok a r = StreamHandler
  { onSuccess     :: StreamState tok -> a -> r -- ^ Success Continuation
  , onEmpty       :: StreamState tok -> r -- ^ Empty continuation
  , onFailure     :: StreamState tok -> Builder -> r -- ^ Failure Continuation
  , onHelpRequest :: StreamState tok -> r -- ^ Help Continuation
  } deriving (Functor)

-- | The amazing stream parsing monad! This monad tracks the stream
-- state and context. It short-circuits when exceptions or
-- help-requests are raised.
newtype StreamParser tok a = StreamParser
  { runStreamParser
    :: forall r. StreamHandler tok a r
    -> StreamState tok
    -> r
  }

instance Functor (StreamParser tok) where
  fmap f parser = StreamParser $ \handler ->
    runStreamParser parser handler { onSuccess = \s -> onSuccess handler s . f }

instance Applicative (StreamParser tok) where
  pure a = StreamParser $ \handler state -> onSuccess handler state a
  mf <*> ma = StreamParser $ \handler ->
    runStreamParser mf
    handler { onSuccess = \s f -> runStreamParser ma handler { onSuccess = \s' -> onSuccess handler s' . f } s }

instance Alternative (StreamParser tok) where
  empty = StreamParser $ \handler -> onEmpty handler
  l <|> r = StreamParser $ \handler ->
    runStreamParser l handler { onEmpty = runStreamParser r handler }

instance Monad (StreamParser tok) where
  return = pure
  ma >>= f = StreamParser $ \handler ->
    runStreamParser ma handler { onSuccess = \s a -> runStreamParser (f a) handler s }

instance MonadError Builder (StreamParser tok) where
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
setEscaped :: Bool -> StreamParser tok ()
setEscaped b = StreamParser $ \handler state ->
  onSuccess handler state { streamEscaped = b } ()

-- | Check whether escaped parsing is enabled.
getEscaped :: StreamParser tok Bool
getEscaped = StreamParser $ \handler state ->
  onSuccess handler state (streamEscaped state)

-- | Signal that help information is requested. Short-circuits any
-- further operations.
requestHelp :: StreamParser tok a
requestHelp = StreamParser onHelpRequest

-- | Get a list representing the current context stack.
getContext :: StreamParser tok [tok]
getContext = StreamParser $ \handler state ->
  onSuccess handler state (streamContext state)

-- | Replace the context stack.
setContext :: [tok] -> StreamParser tok ()
setContext contexts = StreamParser $ \handler state ->
  onSuccess handler state { streamContext = contexts } ()

-- | Push the provided token onto the context stack, then perform some
-- computation. Afterwards, the stack is restored to its prior state.
withContext :: tok -> StreamParser tok a -> StreamParser tok a
withContext context action = do
  oldContext <- getContext
  setContext $ context : oldContext
  action <* setContext oldContext

-- | Format an error message with context information.
renderError :: Render tok => [tok] -> Builder -> Builder
renderError contexts err =
  mconcat
  $ List.intersperse ": "
  $ reverse
  $ err : map render contexts

--------------------------------------------------------------------------------

-- | Remove and return the first token in the stream.
popMaybe :: StreamParser tok (Maybe Text)
popMaybe = StreamParser $ \handler state ->
  case streamContent state of
    (t:ts') -> onSuccess handler state { streamContent = ts' } (Just t)
    _       -> onSuccess handler state Nothing

-- | View the first token in the stream without consuming it.
peekMaybe :: StreamParser tok (Maybe Text)
peekMaybe = StreamParser $ \handler state ->
  case streamContent state of
    (t:_) -> onSuccess handler state (Just t)
    _     -> onSuccess handler state Nothing

-- | Remove and return the first token in the stream. Evaluates to
-- 'empty' if there are no tokens in the stream.
pop :: StreamParser tok Text
pop = StreamParser $ \handler state ->
  case streamContent state of
    (t:ts') -> onSuccess handler state { streamContent = ts' } t
    _       -> onEmpty handler state

-- | View the first token in the stream without consuming it.
-- Evaluates to 'empty' if there are no tokens in the stream.
peek :: StreamParser tok Text
peek = StreamParser $ \handler state ->
  case streamContent state of
    (t:_) -> onSuccess handler state t
    _     -> onEmpty handler state

-- | Prepend a token to the front of the stream.
push :: Text -> StreamParser tok ()
push t = StreamParser $ \handler state ->
  onSuccess handler
  state { streamContent = t : streamContent state }
  ()

-- | Discard the first token in the stream. Nothing happens if there
-- are no tokens in the stream.
pop_ :: StreamParser tok ()
pop_ = StreamParser $ \handler state ->
  onSuccess handler
  state { streamContent = drop 1 $ streamContent state }
  ()

--------------------------------------------------------------------------------

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. When a subtree successfully consumes input, it is
-- replaced with an updated subtree and the traversal ceases.
feed :: Scheme s => ParseTree s r -> StreamParser (Token s) (ParseTree s r)
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
satiate :: Scheme s => ParseTree s r -> StreamParser (Token s) (ParseTree s r)
satiate tree = do
  parseSpecials
  result <- optional $ feed tree
  case result of
    Just tree' -> satiate tree'
    Nothing    -> pure tree
