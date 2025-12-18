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
import           Control.Monad.Trans.Maybe
import           Data.Functor
import           Data.Kind
import qualified Data.List                 as List
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Text                 (Text)
import qualified Data.Text                 as T

-- | Valency represents the maxiumum number of arguments a parser
-- might consume.
--
-- If the valency of a parser is 'Just n', then it consumes up to 'n'
-- arguments. If the valency is 'Nothing', it can consume an arbitrary
-- number of arguments.
class HasValency p where
  valency :: p r -> Maybe Integer

-- | Things that can be resolved to a value, but might fail to
-- resolve.
class Resolve f where
  resolve :: f r -> Except String r

-- | A type class for meant to parameterize 'ParseTree's. A parser can
-- consume input token and produce a result or throw an error.
class Resolve p => Parser (p :: Type -> Type) where
  data Token p
  parseTokens :: [Text] -> [Token p]
  renderTokens :: [Token p] -> [Text]

  accepts :: p r -> Token p -> Bool
  feedParser :: p r -> MaybeT (Stream (Token p)) r

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

instance Resolve p => Resolve (ParseTree p) where
  resolve EmptyNode          = throwError "empty"
  resolve (ValueNode value)  = pure value
  resolve (ParseNode parser) = resolve parser
  resolve (MapNode f p)      = fmap f $ resolve p
  resolve (ProdNode f l r)   = f <$> resolve l <*> resolve r
  resolve (SumNode l r)      = resolve l <|> resolve r
  resolve (ManyNode _)       = pure []
  -- TODO: What if the ManyNode contains a resolvable node (e.g.
  -- `ManyNode (ValueNode 5)`)? Handling it this way avoids infinite
  -- loops, but might not be the expected behavior.

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

-- | Parse a 'Text' of the form "key=value" into ("key", "value"). If
-- the delimiter ('=') does not appear in the string, the result is
-- 'Nothing'.
keyEqualsValue :: Text -> Maybe (Text, Text)
keyEqualsValue s =
  case T.break (== '=') s of
    (key, T.uncons -> Just (_, value)) -> Just (key, value)
    _                                  -> Nothing

-- | Parsers for subarguments of an option, i.e. '--option key=value'.
data SubParser r
  = SubParameter (TextParser r)
  | SubAssoc Text (TextParser r)
  deriving (Functor)

instance HasValency SubParser where
  valency _ = Just 1

instance Resolve SubParser where
  resolve (SubParameter (TextParser hint _)) =
    throwError $ "expected " <> T.unpack hint
  resolve (SubAssoc key (TextParser hint _)) =
    throwError $ "expected " <> T.unpack key <> "=" <> T.unpack hint

instance Parser SubParser where
  data Token SubParser
    = SubKeyValue Text Text -- ^ A key=value argument
    | SubArgument Text -- ^ A standard argument
    deriving (Show)

  parseTokens xs = fmap parse xs
    where
      parse (keyEqualsValue -> Just (k, v)) = SubKeyValue k v
      parse s                               = SubArgument s

  renderTokens xs = fmap unparse xs
    where
      unparse (SubKeyValue k v) = k <> "=" <> v
      unparse (SubArgument s)   = s

  accepts (SubParameter _) (SubArgument _)   = True
  accepts (SubAssoc key _) (SubKeyValue k _) = key == k
  accepts _ _                                = False

  feedParser (SubParameter (TextParser _ parse)) = do
    MaybeT peek >>= \case
      SubArgument s -> lift $ pop *> lift (parse s)
      _             -> empty
  feedParser (SubAssoc key (TextParser _ parse)) = do
    MaybeT peek >>= \case
      SubKeyValue k v | key == k -> lift $ pop *> lift (parse v)
      _                          -> empty

--------------------------------------------------------------------------------
-- Top-level CLI Parsing

-- | If the next token in the stream is an 'Argument', pop it and
-- convert it into an argument list that can be passed to a subparser.
-- `popArgument False` should generate a list of 0 or 1 values, while
-- `popArgument True` comma-splits any argument it finds into multiple
-- values.
popArguments :: Bool -> Stream (Token CliParser) [Text]
popArguments split = state $ \case
  (Argument s : xs') -> (if split
                         then T.split (== ',') s
                         else [s], xs')
  xs -> ([], xs)

-- | Parsers for top-level CLI arguments such as commands and options.
data CliParser r
  = CliParameter (TextParser r)
  | CliOption OptionInfo (ParseTree SubParser r)
  | CliCommand CommandInfo (ParseTree CliParser r)
  deriving (Functor)

instance HasValency CliParser where
  valency (CliParameter _) = Just 1
  valency (CliOption _ subtree) = case valency subtree of
                                    Just n | n <= 0 -> Just 1
                                    _               -> Just 2
  valency (CliCommand _ subtree) = (+1) <$> valency subtree

instance Resolve CliParser where
  resolve (CliParameter (TextParser hint _)) =
    throwError $ "expected " <> T.unpack hint
  resolve (CliOption info _) =
    throwError $ "expected " <> show (optHead info)
  resolve (CliCommand info _) =
    throwError $ "expected " <> show (cmdHead info)

instance Parser CliParser where
  data Token CliParser
    = LongOption Text -- ^ A long form flag (e.g. --option)
    | ShortOption Char -- ^ A short form flag (e.g. -c)
    | Argument Text -- ^ A freeform argument that is not an option
    | Escaped Text -- ^ An argument escaped using '--' that can only
                 -- be consumed by 'CliParameter' parsers.
    deriving (Show)

  parseTokens args =
    let (regularArgs, drop 1 -> escapedArgs) = break (== "--") args
    in fmap argToToken regularArgs <> fmap Escaped escapedArgs
    where
      argToToken (T.stripPrefix "--" -> Just s)                   = LongOption s
      argToToken (T.stripPrefix "-" >=> T.uncons -> Just (c, "")) = ShortOption c
      argToToken s                                                = Argument s

  renderTokens toks = map tokenToArg toks
    where
      tokenToArg (LongOption s)  = "--" <> s
      tokenToArg (ShortOption c) = "-" <> T.singleton c
      tokenToArg (Argument s)    = s
      -- TODO: How should we render escaped tokens?
      tokenToArg (Escaped s)     = s

  accepts (CliParameter _) (Argument _)      = True
  accepts (CliParameter _) (Escaped _)       = True
  accepts (CliOption info _) (LongOption s)  = LongFlag s `elem` optFlags info
  accepts (CliOption info _) (ShortOption c) = ShortFlag c `elem` optFlags info
  accepts (CliCommand info _) (Argument s)   = s `elem` cmdNames info
  accepts _ _                                = False

  feedParser (CliParameter (TextParser _ parse)) = do
    text <- MaybeT peek >>= \case
      Argument s -> pure s
      Escaped s  -> pure s
      _          -> empty
    lift $ pop *> lift (parse text)
  feedParser parser@(CliOption _ subtree) = do
    next <- MaybeT peek
    guard $ parser `accepts` next
    void $ lift pop

    -- Collect the next argument in the stream and marshal it into a
    -- list of tokens for the subcontext
    let isMultary = all (> 1) $ valency subtree
    args <- lift $ parseTokens <$> popArguments isMultary
    (result, _args') <- lift . lift $ withExcept ("CliOption: " <>) $ runParseTree subtree args
    case renderTokens _args' of
      (arg:_) | isMultary -> throwError $ "unrecognized subargument: " <> show arg
      args'               -> traverse (lift . push . Argument) args' $> result
  feedParser parser@(CliCommand _ subtree) = do
    next <- MaybeT peek
    guard $ parser `accepts` next
    lift $ pop
      *> satiate subtree
      >>= lift . resolve

--------------------------------------------------------------------------------
-- Feeding the Tree

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. Once a subtree consumes input, it is replaced with
-- an updated subtree and further traversal ceases.
feed :: Parser p => ParseTree p r -> MaybeT (Stream (Token p)) (ParseTree p r)
feed EmptyNode = empty
feed (ValueNode _) = empty
feed (ParseNode parser) = ValueNode <$> feedParser parser
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
-- no longer consumed.
satiate :: Parser p => ParseTree p r -> Stream (Token p) (ParseTree p r)
satiate tree = do
  result <- runMaybeT $ feed tree
  case result of
    Just tree' -> satiate tree'
    Nothing    -> pure tree

runParseTree
  :: Parser p
  => ParseTree p r
  -> [Token p]
  -> Except String (r, [Token p])
runParseTree tree args = do
  (tree', args') <- runStateT (satiate tree) args
  result <- resolve tree'
  return (result, args')

parseArguments
  :: Parser p
  => ParseTree p r
  -> [Text]
  -> Either String (r, [Token p])
parseArguments tree =
  runExcept
  . runParseTree tree
  . parseTokens
