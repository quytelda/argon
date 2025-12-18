{-# LANGUAGE DeriveFunctor     #-}
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
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Parser
import           ParseTree
import           Stream

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

runTextParser :: TextParser r -> Text -> StreamParser tok r
runTextParser (TextParser _ parse) = lift . lift . parse

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

  parseTokens = fmap parse
    where
      parse (keyEqualsValue -> Just (k, v)) = SubKeyValue k v
      parse s                               = SubArgument s

  renderTokens = fmap unparse
    where
      unparse (SubKeyValue k v) = k <> "=" <> v
      unparse (SubArgument s)   = s

  accepts (SubParameter _) (SubArgument _)   = True
  accepts (SubAssoc key _) (SubKeyValue k _) = key == k
  accepts _ _                                = False

  feedParser (SubParameter tp) = do
    peekP >>= \case
      SubArgument s -> popP *> runTextParser tp s
      _             -> empty
  feedParser (SubAssoc key tp) = do
    peekP >>= \case
      SubKeyValue k v | key == k -> popP *> runTextParser tp v
      _                          -> empty

--------------------------------------------------------------------------------
-- Top-level CLI Parsing

-- | If the next token in the stream is an 'Argument', pop it and
-- convert it into an argument list that can be passed to a subparser.
-- `popArgument False` should generate a list of 0 or 1 values, while
-- `popArgument True` comma-splits any argument it finds into multiple
-- values.
popArguments :: Bool -> StreamParser (Token CliParser) [Text]
popArguments split = lift . state $ \case
  (Bound s : xs')    -> (asList s, xs')
  (Argument s : xs') -> (asList s, xs')
  xs                 -> ([], xs)
  where
    asList s = if split
               then T.split (== ',') s
               else [s]

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
    | Bound Text -- ^ A subargument bound to an option (e.g. --opt=ARG)
    | Argument Text -- ^ A freeform argument that is not an option
    | Escaped Text -- ^ An argument escaped using '--' that can only
                   -- be consumed by 'CliParameter' parsers.
    deriving (Show)

  parseTokens args =
    let (regularArgs, drop 1 -> escapedArgs) = break (== "--") args
    in concatMap argToTokens regularArgs <> fmap Escaped escapedArgs
    where
      argToTokens (T.stripPrefix "--" -> Just s) =
        case keyEqualsValue s of
          Just (k, v) -> [LongOption k, Bound v]
          Nothing     -> [LongOption s]
      argToTokens (T.stripPrefix "-" >=> T.uncons -> Just (c, "")) = [ShortOption c]
      argToTokens s                                                = [Argument s]

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

  feedParser (CliParameter tp) = do
    text <- peekP >>= \case
      Argument s -> pure s
      Escaped s  -> pure s
      _          -> empty
    popP *> runTextParser tp text
  feedParser parser@(CliOption _ subtree) = do
    next <- peekP
    guard $ parser `accepts` next
    void $ lift pop

    -- Collect the next argument in the stream and marshal it into a
    -- list of tokens for the subcontext
    let isMultary = all (> 1) $ valency subtree
    args <- popArguments isMultary <&> parseTokens
    (result, _args') <- lift . lift $ runParseTree subtree args
    case renderTokens _args' of
      (arg:_) | isMultary -> throwError $ "unrecognized subargument: " <> show arg
      args'               -> traverse (pushP . Argument) args' $> result
  feedParser parser@(CliCommand _ subtree) = do
    next <- peekP
    guard $ parser `accepts` next

    popP
      *> lift (satiate subtree)
      >>= resolveP

--------------------------------------------------------------------------------
-- Feeding the Tree

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. Once a subtree consumes input, it is replaced with
-- an updated subtree and further traversal ceases.
feed :: Parser p => ParseTree p r -> StreamParser (Token p) (ParseTree p r)
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
