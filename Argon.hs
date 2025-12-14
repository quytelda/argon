{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Argon where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Functor
import qualified Data.List                 as List
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Text                 (Text)
import qualified Data.Text                 as T

-- | 'ParseTree p r' is an expression tree built from parsers of type
-- 'p' which evaluates to a value of type 'r' supplied with the proper
-- input.
data ParseTree p r where
  EmptyNode :: ParseTree p r -- ^ Terminal node with no value
  ValueNode :: r -> ParseTree p r -- ^ Terminal node with a resolved value
  ParseNode :: p r -> ParseTree p r -- ^ A parser awaiting input
  MapNode :: (a -> r) -> ParseTree p a -> ParseTree p r -- ^ Abstracts fmap
  ProdNode :: (u -> v -> r) -> ParseTree p u -> ParseTree p v -> ParseTree p r -- ^ Abstracts liftA2
  SumNode :: ParseTree p r -> ParseTree p r -> ParseTree p r -- ^ Abstracts (<|>)
  ManyNode :: ParseTree p r -> ParseTree p [r] -- ^ Abstracts many

instance Functor (ParseTree p) where
  fmap f = MapNode f

instance Applicative (ParseTree p) where
  pure = ValueNode
  liftA2 = ProdNode

instance Alternative (ParseTree p) where
  empty = EmptyNode
  (<|>) = SumNode
  many = ManyNode

-- | Things that can be resolved to a value, but might fail to
-- resolve.
class Resolve f where
  resolve :: f r -> Except String r

instance Resolve p => Resolve (ParseTree p) where
  resolve EmptyNode          = throwError "empty"
  resolve (ValueNode value)  = pure value
  resolve (ParseNode parser) = resolve parser
  resolve (MapNode f p)      = fmap f $ resolve p
  resolve (ProdNode f l r)   = f <$> resolve l <*> resolve r
  resolve (SumNode l r)      = resolve l <|> resolve r
  resolve (ManyNode p)       = pure []

--------------------------------------------------------------------------------
-- Valency

-- | 'Arity' represents the number of input a parser-like object might
-- consume. We only care about three classes: 'Nullary' parsers
-- consume no input, 'Unary' parsers consume up to one input, and
-- 'Multary' parsers can potentially consume two or more inputs.
data Arity = Nullary | Unary | Multary
  deriving (Eq, Show, Ord)

-- | 'Arity's can be combined somewhat like addition.
instance Semigroup Arity where
  Nullary <> Nullary = Nullary
  Nullary <> Unary   = Unary
  Unary   <> Nullary = Unary
  _       <> _       = Multary

-- | Parser-like objects that can be analyzed to determine
-- valency/arity.
class Valency p where
  valency :: p r -> Arity

  -- | While we need to track three possible arity cases to correctly
  -- compute the valency of a parse tree, we mostly only care whether
  -- the tree is 'Multary'. This convenience function tells us whether
  -- or not the tree is multary.
  multary :: p r -> Bool
  multary parser = valency parser == Multary

instance Valency p => Valency (ParseTree p) where
  valency EmptyNode        = Nullary
  valency (ValueNode _)    = Nullary
  valency (ParseNode _)    = Unary
  valency (MapNode _ p)    = valency p
  valency (ProdNode _ l r) = valency l <> valency r
  valency (SumNode l r)    = max (valency l) (valency r)
  valency (ManyNode _)     = Multary

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
-- Tokens

-- | CLI Argument Tokens
data Token
  = LongOption Text -- ^ A long form flag (e.g. --option)
  | ShortOption Char -- ^ A short form flag (e.g. -c)
  | Argument Text -- ^ A freeform argument that is not an option
  | Escaped Text -- ^ An argument escaped using '--' that can only
                 -- be consumed by 'CliParameter' parsers.
  deriving (Show)

tokenize :: [Text] -> [Token]
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
popArguments :: Bool -> Stream Token [Text]
popArguments split = state $ \case
  (Argument s : xs') -> (listify s, xs')
  xs -> ([], xs)
  where
    listify s =
      if split
      then T.split (== ',') s
      else [s]

--------------------------------------------------------------------------------

class Accepts a where
  accepts :: a -> Token -> Bool

data Flag
  = LongFlag Text
  | ShortFlag Char
  deriving (Eq, Show)

data OptionInfo = OptionInfo
  { optFlags :: NonEmpty Flag
  , optHelp  :: Text
  } deriving (Show)

instance Accepts OptionInfo where
  accepts OptionInfo{..} (LongOption s) =
    LongFlag s `elem` optFlags
  accepts OptionInfo{..} (ShortOption c) =
    ShortFlag c `elem` optFlags
  accepts _ _ = False

-- | Get a representative flag for this option (e.g. the first one).
optHead :: OptionInfo -> Flag
optHead = NonEmpty.head . optFlags

data CommandInfo = CommandInfo
  { cmdNames :: NonEmpty Text
  , cmdHelp  :: Text
  } deriving (Show)

instance Accepts CommandInfo where
  accepts CommandInfo{..} (Argument s) = s `elem` cmdNames
  accepts _ _                          = False

-- | Get a representative command name for this command (e.g. the
-- first one).
cmdHead :: CommandInfo -> Text
cmdHead = NonEmpty.head . cmdNames

--------------------------------------------------------------------------------

-- | The result of attempting to activate a parser
data ParserResult p r
  = Empty -- ^ The parser consumed no input (i.e. does not apply)
  | Partial (p r) -- ^ The parser consumed input and requires more
  | Done r -- ^ The parser consumed input and completed
  deriving (Functor)

mapParser :: (p r -> q r) -> ParserResult p r -> ParserResult q r
mapParser f Empty            = Empty
mapParser f (Done r)         = Done r
mapParser f (Partial parser) = Partial $ f parser

eitherToResult :: MonadError String m => Either String r -> m (ParserResult p r)
eitherToResult = either throwError (pure . Done)

-- | A type class for anything that can be a parsing node in a
-- 'ParseTree'. A 'Parser tok p' processes a stream of 'tok's.
class Parser tok p where
  feedParser :: p r -> Stream tok (ParserResult p r)

-- | A parser that accepts a simple text argument.
data TextParser r = TextParser Text (Text -> Either String r)
  deriving (Functor)

instance Valency TextParser where
  valency _ = Unary

instance Resolve TextParser where
  resolve (TextParser hint _) =
    throwError $ "TextParser: expected " <> T.unpack hint

-- | Parsers for the argument of an option, i.e. '--option key=value'.
data OptParser r
  = OptParameter (TextParser r) -- ^ A standard parameter
  | OptKey Text (TextParser r) -- ^ A key=value parameter
  | OptSwitch Text r -- ^ A switch is either present or absent
  deriving (Functor)

instance Valency OptParser where
  valency (OptParameter p) = valency p
  valency (OptKey _ p)     = valency p
  valency (OptSwitch _ _)  = Unary

instance Resolve OptParser where
  resolve (OptParameter parser) = withExcept ("OptParameter: " <>) $ resolve parser
  resolve (OptKey key parser) =
    withExcept (\s -> "OptKey (" <> T.unpack key <> "): " <> s) $ resolve parser
  resolve (OptSwitch name _) = throwError $ "OptSwitch: " <> T.unpack name

-- | Parse a 'Text' of the form "key=value" into ("key", "value"). If
-- the delimiter ('=') does not appear in the string, the result is
-- 'Nothing'.
keyEqualsValue :: Text -> Maybe (Text, Text)
keyEqualsValue s =
  case T.break (== '=') s of
    (key, T.uncons -> Just (_, value)) -> Just (key, value)
    _                                  -> Nothing

-- TODO: The OptParameter case will consume any argument, even if it
-- is a valid key=value argument that should be consumed by a
-- subsequent parser (or a switch). Can we deal with this somehow?
instance Parser Text OptParser where
  feedParser (OptParameter (TextParser hint parse)) = pop >>= \case
    Just s -> eitherToResult (parse s)
    Nothing -> pure Empty
  feedParser (OptKey key (TextParser _ parse)) = peek >>= \case
    Just (keyEqualsValue -> Just (k, v))
      | key == k -> pop *> eitherToResult (parse v)
    _ -> pure Empty
  feedParser (OptSwitch key present) = peek >>= \case
    Just s | key == s -> pop $> Done present
    _ -> pure Empty

-- | Parsers for top-level CLI arguments such as commands and options.
data CliParser r
  = CliParameter (TextParser r) -- ^ A simple parameter
  | CliOption OptionInfo (ParseTree OptParser r) -- ^ An option (e.g. '--option')
  | CliCommand CommandInfo (ParseTree CliParser r) -- ^ A subcommand
  deriving (Functor)

instance Valency CliParser where
  valency (CliParameter p)    = valency p
  valency (CliOption _ tree)  = Unary <> valency tree
  valency (CliCommand _ tree) = Unary <> valency tree

instance Resolve CliParser where
  resolve (CliParameter parser) = withExcept ("CliParameter: " <>) $ resolve parser
  resolve (CliOption info _)    = throwError $ "CliOption (" <> show (optHead info) <> ")"
  resolve (CliCommand info _)   = throwError $ "CliCommand (" <> show (cmdHead info) <> ")"

instance Parser Token CliParser where
  feedParser (CliParameter (TextParser hint parse)) = peek >>= \case
    Just (Argument s) -> pop *> eitherToResult (parse s)
    Just (Escaped s)  -> pop *> eitherToResult (parse s)
    _ -> pure Empty
  feedParser (CliOption info tree) = peek >>= \case
    Just s | info `accepts` s -> do
          -- consume option flag
          void pop

          args <- popArguments (multary tree)
          lift (parseTokens tree args) >>= \case
            (_, arg:_)
              | multary tree ->
                throwError $ "unrecognized subargument: " <> show arg
            (result, args') ->
              traverse (push . Argument) args' $> Done result
    _ -> pure Empty
  feedParser (CliCommand info tree) = peek >>= \case
    Just s | info `accepts` s ->
             pop -- consume command
             *> satiate tree
             >>= lift . resolve
             <&> Done
    _ -> pure Empty

--------------------------------------------------------------------------------
-- Feeding the Tree

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. Once a subtree consumes input, it is replaced with
-- an updated subtree and further traversal ceases.
feed :: Parser tok p => ParseTree p r -> MaybeT (Stream tok) (ParseTree p r)
feed EmptyNode = empty
feed (ValueNode _) = empty
feed (ParseNode parser) = lift (feedParser parser) >>= \case
  Done value      -> pure $ ValueNode value
  Partial parser' -> pure $ ParseNode parser'
  Empty -> empty
feed (MapNode f tree) = MapNode f <$> feed tree
feed (ProdNode f l r) =
  (ProdNode f <$> feed l <*> pure r) <|>
  (ProdNode f l <$> feed r)
feed (SumNode l r) =
  (SumNode <$> feed l <*> pure r) <|>
  (SumNode l <$> feed r)
feed (ManyNode tree) =
  ProdNode (:)
  <$> feed tree
  <*> pure (ManyNode tree)

-- | Repeatedly feed input to the tree using `feed` until no input is
-- consumed (e.g. `feed` returns Nothing).
satiate :: Parser tok p => ParseTree p r -> Stream tok (ParseTree p r)
satiate tree = do
  result <- runMaybeT $ feed tree
  case result of
    Just tree' -> satiate tree'
    Nothing    -> pure tree

parseTokens
  :: (Parser tok p, Resolve p)
  => ParseTree p a
  -> [tok]
  -> Except String (a, [tok])
parseTokens tree args = do
  (tree', args') <- runStateT (satiate tree) args
  result <- resolve tree'
  return (result, args')

parseArguments
  :: (Parser Token p, Resolve p)
  => ParseTree p a
  -> [Text]
  -> Either String (a, [Token])
parseArguments tree = runExcept . parseTokens tree . tokenize
