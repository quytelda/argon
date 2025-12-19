{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Lib where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Parser
import           ParseTree
import           Stream
import           Text

--------------------------------------------------------------------------------
-- User Interface Descriptions

data Flag
  = LongFlag Text
  | ShortFlag Char
  deriving (Eq, Show)

instance Render Flag where
  render (LongFlag s)  = "--" <> render s
  render (ShortFlag c) = "-" <> render c

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

parserHint :: TextParser r -> Text
parserHint (TextParser hint _) = hint

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

instance Render (SubParser r) where
  render (SubParameter tp) = render $ parserHint tp
  render (SubAssoc key tp) = render key <> "=" <> render (parserHint tp)

instance Parser SubParser where
  data Token SubParser
    = SubKeyValue Text Text -- ^ A key=value argument
    | SubArgument Text -- ^ A standard argument
    deriving (Show)

  parseTokens = fmap parse
    where
      parse (keyEqualsValue -> Just (k, v)) = SubKeyValue k v
      parse s                               = SubArgument s

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

instance Render (Token SubParser) where
  render (SubKeyValue k v) = render k <> "=" <> render v
  render (SubArgument s)   = render s

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

instance Render (Token CliParser) where
  render (LongOption s)  = "--" <> render s
  render (ShortOption c) = "-" <> render c
  render (Bound s)       = "subargument \"" <> render s <> "\""
  render (Argument s)    = render s
  render (Escaped s)     = render s

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

    -- Collect arguments for the subparser's stream from the next
    -- argument in the parent stream.
    let asList s = if valencyIs (> 1) subtree
                   then T.split (== ',') s
                   else [s]

    args <- lift peek <&> \case
      Just (Bound s)    -> asList s
      Just (Argument s) -> asList s
      _                 -> []

    -- Evaluate the subparser in a new stream context.
    (result, leftovers) <- liftEither $ parseArguments subtree args

    -- If the subparser consumed its input, we can safely remove it
    -- the from the parent stream. However, we cannot remove partially
    -- consumed input, so in that case we throw an error.
    when (length args /= length leftovers) $
      popP *> mapM_ (\arg -> throwError $ "unrecognized subargument: " <> show arg) leftovers

    -- Ensure we are not leaving an unconsumed bound argument at the
    -- head of the stream.
    lift peek >>= \case
      Just (Bound s) -> throwError $ "unrecognized subargument: " <> show s
      _ -> pure ()

    pure result
  feedParser parser@(CliCommand _ subtree) = do
    next <- peekP
    guard $ parser `accepts` next

    popP
      *> lift (satiate subtree)
      >>= resolveP
