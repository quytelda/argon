{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Lib where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Kind
import qualified Data.List                 as List
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Text                 (Text)
import qualified Data.Text                 as T

class HasValency p where
  valency :: p r -> Maybe Integer

-- | A type class for meant to parameterize 'ParseTree's. A parser can
-- consume input token and produce a result or throw an error.
class Parser (p :: Type -> Type) where
  type Token p

--------------------------------------------------------------------------------
-- ParseTree

-- | 'ParseTree p r' is an expression tree built from parsers of type
-- 'p' which evaluates to a value of type 'r' supplied with the proper
-- input.
data ParseTree p r where
  -- | Terminal node with no value
  EmptyNode :: ParseTree p r
  -- | Terminal node with a resolved value
  ValueNode :: r -> ParseTree p r
  -- | A parser awaiting input
  ParseNode :: p r -> ParseTree p r
  -- | Abstracts fmap
  MapNode :: (a -> r) -> ParseTree p a -> ParseTree p r
  -- | Abstracts liftA2
  ProdNode :: (u -> v -> r) -> ParseTree p u -> ParseTree p v -> ParseTree p r
  -- | Abstracts (<|>)
  SumNode :: ParseTree p r -> ParseTree p r -> ParseTree p r
  -- | Abstracts many
  ManyNode :: ParseTree p r -> ParseTree p [r]

instance Functor (ParseTree p) where
  fmap f = MapNode f

instance Applicative (ParseTree p) where
  pure = ValueNode
  liftA2 = ProdNode

instance Alternative (ParseTree p) where
  empty = EmptyNode
  (<|>) = SumNode
  many = ManyNode

instance HasValency p => HasValency (ParseTree p) where
  valency EmptyNode        = Just 0
  valency (ValueNode _)    = Just 0
  valency (ParseNode p)    = valency p
  valency (MapNode _ p)    = valency p
  valency (ProdNode _ l r) = (+) <$> valency l <*> valency r
  valency (SumNode l r)    = max <$> valency l <*> valency r
  valency (ManyNode p)     = case valency p of
                               Just n | n <= 0 -> Just 0
                               _               -> Nothing

--------------------------------------------------------------------------------
-- Stream Monad

-- | A stream parameterized by token type.
type Stream tok = StateT [tok] (Except String)

runStream :: Stream tok a -> [tok] -> Either String (a, [tok])
runStream m = runExcept . runStateT m

isEmptyStream :: Stream tok Bool
isEmptyStream = gets null

pop :: Stream tok (Maybe tok)
pop = state $ \case
  [] -> (Nothing, [])
  (x:xs) -> (Just x, xs)

peek :: Stream tok (Maybe tok)
peek = gets $ fmap fst . List.uncons

push :: tok -> Stream tok ()
push s = modify' (s:)

--------------------------------------------------------------------------------
-- User Interface Descriptions

data Flag
  = LongFlag Text
  | ShortFlag Char
  deriving (Eq, Show)

data OptionInfo = OptionInfo
  { optFlags :: NonEmpty Flag
  , optHelp  :: Text
  } deriving (Show)

-- | Get a representative flag for this option (e.g. the first one).
optHead :: OptionInfo -> Flag
optHead = NonEmpty.head . optFlags

data CommandInfo = CommandInfo
  { cmdNames :: NonEmpty Text
  , cmdHelp  :: Text
  } deriving (Show)

-- | Get a representative command name for this command (e.g. the
-- first one).
cmdHead :: CommandInfo -> Text
cmdHead = NonEmpty.head . cmdNames

--------------------------------------------------------------------------------
-- Basic Parser

data TextParser r = TextParser Text (Text -> Except String r)
  deriving (Functor)

--------------------------------------------------------------------------------
-- Subargument Parsing

data SubToken
  = SubKeyValue Text Text -- ^ A key=value argument
  | SubArgument Text -- ^ A standard argument
  deriving (Eq, Show)

-- | Parsers for subarguments of an option, i.e. '--option key=value'.
data SubParser r
  = SubParameter (TextParser r)
  | SubAssoc Text (TextParser r)
  deriving (Functor)

instance HasValency SubParser where
  valency _ = Just 1

--------------------------------------------------------------------------------
-- Top-level CLI Parsing

data CliToken
  = LongOption Text -- ^ A long form flag (e.g. --option)
  | ShortOption Char -- ^ A short form flag (e.g. -c)
  | Argument Text -- ^ A freeform argument that is not an option
  | Escaped Text -- ^ An argument escaped using '--' that can only
                 -- be consumed by 'CliParameter' parsers.
  deriving (Show)

tokenize :: [Text] -> [CliToken]
tokenize args =
  let (regularArgs, drop 1 -> escapedArgs) = break (== "--") args
  in fmap argToToken regularArgs <> fmap Escaped escapedArgs
  where
    argToToken (T.stripPrefix "--" -> Just s)                   = LongOption s
    argToToken (T.stripPrefix "-" >=> T.uncons -> Just (c, "")) = ShortOption c
    argToToken s                                                = Argument s

-- | If the next token in the stream is an 'Argument', pop it and
-- convert it into an argument list that can be passed to a subparser.
-- `popArgument False` should generate a list of 0 or 1 values, while
-- `popArgument True` comma-splits any argument it finds into multiple
-- values.
popArguments :: Bool -> Stream CliToken [Text]
popArguments split = state $ \case
  (Argument s : xs') -> (if split
                         then T.split (== ',') s
                         else [s], xs')
  xs -> ([], xs)
