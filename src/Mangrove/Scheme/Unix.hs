{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : Mangrove.Scheme.Unix
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A parsing scheme for Unix-style command line arguments.
-}
module Mangrove.Scheme.Unix
  ( -- * Describing Commands & Options
    Flag(..)
  , OptionInfo(..)
  , CommandInfo(..)

    -- * Unix Scheme
  , UnixScheme(..)
  , Token(..)
  , UnixParser

    -- * Help
  , addHelpOptions
  , renderHelp
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import qualified Data.List              as List
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.List.NonEmpty     as NonEmpty
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

import           Mangrove
import           Mangrove.Parser
import           Mangrove.Resolve
import           Mangrove.Scheme.Sub    (SubScheme)
import qualified Mangrove.Scheme.Sub    as Sub
import           Mangrove.Separable
import           Mangrove.Text
import           Mangrove.TextParser
import           Mangrove.Valency

--------------------------------------------------------------------------------
-- User Interface Descriptions

-- | A flag is a special argument that identifies a named option to
-- the parser. Flags can have two forms: long flags start with a
-- double dash (e.g. "--example") followed by a string while short
-- flags start with only a single dash (e.g. "-e") and are identified
-- by a single character.
--
-- For convenience, 'Flag' is an instance of 'Data.String.IsString'.
-- Thus, you can write @"--flop"@ instead of @LongFlag "flop"@ and
-- @"-c"@ instead of @ShortFlag \'c\'@.
data Flag
  = LongFlag !Text
  | ShortFlag !Char
  deriving (Eq, Ord, Show)

instance IsString Flag where
  fromString ('-':'-':name)
    | not (null name) = LongFlag $ T.pack name
  fromString ['-', c]
    | c /= '-' = ShortFlag c
  fromString s = error $ "not a valid flag: " <> s

instance Render Flag where
  render (LongFlag s)  = "--" <> render s
  render (ShortFlag c) = "-" <> render c

-- | A description of a CLI option.
data OptionInfo = OptionInfo
  { optFlags :: !(NonEmpty Flag) -- ^ A list of flags that trigger this option.
  , optHelp  :: !Text -- ^ A description displayed in help output.
  } deriving (Eq, Ord, Show)

-- | Get a representative flag for this option (e.g. the first one).
optHead :: OptionInfo -> Flag
optHead = NonEmpty.head . optFlags

-- | A description of a CLI command.
data CommandInfo = CommandInfo
  { cmdNames :: !(NonEmpty Text) -- ^ Command Names
  , cmdHelp  :: !Text -- ^ A description displayed in help output.
  } deriving (Eq, Ord, Show)

-- | Get a representative command name for this command (e.g. the
-- first one).
cmdHead :: CommandInfo -> Text
cmdHead = NonEmpty.head . cmdNames

-- | A parsing scheme for Unix-style command line syntax.
data UnixScheme r
  -- | A freeform positional parameter
  = Parameter (TextParser r)
  -- | A subcommand with its own parse tree
  | Command !CommandInfo (ParseTree UnixScheme r)
  -- | A named option that might support suboptions
  | Option !OptionInfo (ParseTree SubScheme r)
  -- | A special option that requests help information
  | HelpOption !OptionInfo
  deriving (Functor)

instance Valency UnixScheme where
  valency (Parameter _)       = Just 1
  valency (Command _ subtree) = fmap (+1) (valency subtree)
  valency (Option _ subtree)  = fmap (max 2) (valency subtree)
  valency (HelpOption _)      = Just 1

instance Resolve UnixScheme where
  resolve (Parameter (TextParser hint _)) =
    ExpectedError [render hint]
  resolve (Option info _) =
    ExpectedError [render $ optHead info]
  resolve (HelpOption info) =
    ExpectedError [render $ optHead info]
  resolve (Command info _) =
    ExpectedError [render $ cmdHead info]

instance Separable UnixScheme where
  separate p@(HelpOption _) = Exhibit Nothing [Modal True p]
  separate (Command info subtree) =
    Exhibit Nothing $ (Modal False <$> maybeToList mregular) <> modals
    where
      Exhibit mregular modals = Command info <$> separate subtree
  separate p = Exhibit (Just p) []

-- | A parser for interpreting options. An option always begins with a
-- flag, followed optionally by an "=" sign and a bound argument. The
-- strings "--" and "-" are not treated as options.
parseUnixOption :: Alternative f => Text -> f (Flag, Maybe Text)
parseUnixOption (T.stripPrefix "--" -> Just s)
  | not (T.null s) =
    case keyEqualsValue s of
      Just (k, v) -> pure (LongFlag k, Just v)
      Nothing     -> pure (LongFlag s, Nothing)
parseUnixOption (T.stripPrefix "-" >=> T.uncons -> Just (k,v))
  | k /= '-' =
    pure (ShortFlag k, if T.null v then Nothing else Just v)
parseUnixOption _ = empty

-- | Does this text look like a flag? We check whether it starts with
-- "-" followed by any other character.
isMarked :: Text -> Bool
isMarked "-" = False
isMarked s   = "-" `T.isPrefixOf` s

instance Scheme UnixScheme where
  data Token UnixScheme
    -- | A freeform positional argument that is not an option or command
    = UnixArgument Text
    -- | A recognized subcommand
    | UnixCommand Text
    -- | A named option with optional bound argument
    | UnixOption Flag (Maybe Text)
    deriving (Eq, Show)

  type HelpSupport UnixScheme = 'Helpful

  delimiter _ = ' '

  parseSpecials = do
    peekMaybe >>= \case
      Just "--" -> pop_ *> setEscaped True
      _         -> pure ()

  activate (Parameter tp) = do
    next <- peek

    -- Arguments that begin with a dash should never be treated as
    -- unbound subarguments. However, the string "-" is always
    -- accepted since this is commonly used to represent stdin.
    escaped <- getEscaped
    guard $ escaped || not (isMarked next)

    withContext (UnixArgument next) $
      pop_ *> runTextParser tp next

  activate (Option info subtree) = do
    -- Arguments should never be interpreted as options when escaped.
    getEscaped >>= guard . not

    (flag, mbound) <- peek >>= parseUnixOption
    guard $ flag `elem` optFlags info
    pop_

    -- We need to convert whatever argument string we have (if any)
    -- into a list of subarguments as input for the subparser. If the
    -- subtree accepts multiple arguments, we split the input by
    -- comma. Otherwise, we can just pass a singleton list containing
    -- the argument string.
    --
    -- If the subtree contains no suboptions, we enable escaping to
    -- prevent arguments containing an "=" sign from being interpreted
    -- as suboptions. This is necessary because individual
    -- subparameter parsers have no way to determine that such an
    -- argument won't be consumed by a subsequent suboption parser.
    -- Escaping forces subparameter parsers to consume the argument,
    -- regardless of its form.
    let splitArgs s = if multary subtree
                      then T.split (== ',') s
                      else [s]
        initState args = StreamState
          { streamContent = args
          , streamContext = []
          , streamEscaped = not $ Sub.hasSubOptions subtree
          }
        parseSubargs args =
          runArgumentParser' subtree (initState args)
          (curry pure)
          (throwError . render)
          NoHelp

    withContext (UnixOption flag mbound) $ do
      -- If a bound argument (e.g. --floop=blah) is provided, we
      -- expect it to be consumed by the subparser. If it isn't fully
      -- consumed, we have nothing to do with the leftovers, so we
      -- throw an error.
      --
      -- If there's no bound argument but the next regular argument
      -- doesn't look like an option, then we try running the
      -- subparser using that as input. If it is fully consumed, we
      -- pop it from the front of the stream. If nothing is consumed,
      -- we leave it at the head of the stream. However, if it is
      -- partially consumed, then something has gone wrong, and we
      -- throw an error.
      mnext <- peekMaybe
      case (mbound, mnext) of
        (Just argString, _) -> do
          (leftover, result) <- parseSubargs (splitArgs argString)
          forM_ leftover $ \arg ->
            throwError $ "unrecognized subargument: " <> render arg
          pure result
        (_, Just argString)
          | not (isMarked argString) -> do
              let args = splitArgs argString
              (leftover, result) <- parseSubargs args
              when (length args /= length leftover) $ do
                forM_ leftover $ \arg ->
                  throwError $ "unrecognized subargument: " <> render arg
                pop_
              pure result
        _ -> do
          (_, result) <- parseSubargs []
          pure result

  activate (HelpOption info) = do
    -- Arguments should never be interpreted as options when escaped.
    getEscaped >>= guard . not

    (flag, mbound) <- peek >>= parseUnixOption
    guard $ flag `elem` optFlags info
    pop_

    withContext (UnixOption flag mbound)
      requestHelp

  activate (Command info subtree) = do
    -- Arguments should never be interpreted as commands when escaped.
    getEscaped >>= guard . not

    next <- peek
    guard $ next `elem` cmdNames info
      && not ("-" `T.isPrefixOf` next) -- not sure if this check is necessary?
    pop_

    withContext (UnixCommand next) $ do
      satiate subtree
      >>= resolveLifted

  usageInfo (Parameter tp) = render $ parserHint tp
  usageInfo (Command info subtree) =
    "{" <> render (cmdHead info) <> " " <> render subtree <> "}"
  usageInfo (Option info subtree) =
    render flag
    <> if nullary subtree
       then mempty
       else separator <> renderDelimitedIf braces isChoice subtree
    where flag = optHead info
          separator = case flag of
                        LongFlag _ -> "="
                        _          -> ""
  usageInfo (HelpOption info) =
    render (optHead info)

instance Render (Token UnixScheme) where
  render (UnixArgument s)                      = render s
  render (UnixCommand s)                       = render s
  render (UnixOption f Nothing)                = render f
  render (UnixOption f@(LongFlag _) (Just v))  = render f <> "=" <> render v
  render (UnixOption f@(ShortFlag _) (Just v)) = render f <> render v

instance SupportsHelp UnixScheme where
  makeHelpInfo tree context name desc = renderText
    $ "Usage:\n"
    <> renderUsages tree <> "\n"
    <> render desc <> "\n"
    <> renderHelp tree context
    where
      renderUsageLine s = render name <> " " <> render s <> "\n"
      renderUsages = foldMap renderUsageLine . exhibitToList . separate

-- | Convenient type alias for Unix-flavored parse trees.
type UnixParser = ParseTree UnixScheme

--------------------------------------------------------------------------------
-- Help

-- | Automatically insert a help option at the top level of the tree
-- and every subcommand tree.
addHelpOptions
  :: NonEmpty Flag
  -> Text
  -> ParseTree UnixScheme r
  -> ParseTree UnixScheme r
addHelpOptions flags desc tree = ParseNode helpOption <|> go tree
  where
    helpOption :: UnixScheme a
    helpOption = HelpOption $ OptionInfo flags desc

    go :: ParseTree UnixScheme a -> ParseTree UnixScheme a
    go (ParseNode (Command info subtree)) =
      ParseNode
      $ Command info
      $ ParseNode helpOption <|> go subtree
    go (ProdNode f l r) = ProdNode f (go l) (go r)
    go (SumNode l r) = SumNode (go l) (go r)
    go (ManyNode require p) = ManyNode require (go p)
    go node = node

data OptionHelp = OptionHelp
  { colShorts :: !TL.Text -- Column 1
  , colLongs  :: !TL.Text -- Column 2
  , colArg    :: !TL.Text -- Column 3
  , colDesc   :: !TL.Text -- Column 4
  } deriving (Eq, Ord, Show)

makeOptionHelp :: OptionInfo -> ParseTree SubScheme r -> OptionHelp
makeOptionHelp OptionInfo{..} subtree =
  OptionHelp
  { colLongs  = fmtFlagList longs
  , colShorts = fmtFlagList shorts
  , colArg    = if nullary subtree
                then mempty
                else renderLazyText subtree
  , colDesc   = TL.fromStrict optHelp
  }
  where
    isLongFlag LongFlag{} = True
    isLongFlag _          = False
    (longs, shorts) = NonEmpty.partition isLongFlag optFlags
    fmtFlagList = TL.intercalate ", " . fmap renderLazyText

-- | Enumerate descriptive information for all options available in a
-- parse tree, indexed by the set of commands under which they exist.
collectOptions :: ParseTree UnixScheme r -> Map [CommandInfo] [OptionHelp]
collectOptions tree = go tree mempty
  where
    go :: ParseTree UnixScheme r
       -> Map [CommandInfo] [OptionHelp]
       -> Map [CommandInfo] [OptionHelp]
    go (ParseNode (Option info subtree)) =
      Map.insertWith (<>) [] [makeOptionHelp info subtree]
    go (ParseNode (Command info subtree)) =
      Map.union $ Map.mapKeys (info :) $ collectOptions subtree
    go (ProdNode _ l r) = go r . go l
    go (SumNode l r)    = go r . go l
    go (ManyNode _ p)   = go p
    go _                = id

renderOptionTable :: [OptionHelp] -> Builder
renderOptionTable xs = foldMap formatRow $ List.sort xs
  where
    maxLengthBy f = maximum $ TL.length . f <$> xs
    col1width = maxLengthBy colShorts
    col2width = maxLengthBy colLongs
    col3width = maxLengthBy colArg

    formatRow OptionHelp{..} =
      TLB.fromLazyText $ TL.intercalate "  "
      [ TL.justifyLeft col1width ' ' colShorts
      , TL.justifyLeft col2width ' ' colLongs
      , TL.justifyLeft col3width ' ' colArg
      , colDesc
      , "\n"
      ]

renderHeader :: [CommandInfo] -> Builder
renderHeader [] = mempty
renderHeader cmds@(info : _) =
  fmtCommand cmds
  <> " command"
  <> aliasInfo
  <> ": "
  <> render (cmdHelp info)
  <> "\n"
  where
    quote m = "\"" <> m <> "\""
    fmtCommand = quote . render . T.unwords . fmap cmdHead . reverse
    aliases = NonEmpty.tail $ cmdNames info
    aliasInfo =
      if null aliases
      then mempty
      else " (alt: " <> render (T.intercalate ", " aliases) <> ")"

-- | Format an index of commands and options for help output display.
renderTables :: Map [CommandInfo] [OptionHelp] -> Builder
renderTables =
  Map.foldlWithKey
  (\acc cmds desc ->
      acc
      <> "\n"
      <> renderHeader cmds
      <> renderOptionTable desc
  ) mempty

-- | Select only the options tables which exist under a particular
-- command sequence.
selectSubtable
  :: [Text]
  -> Map [CommandInfo] [OptionHelp]
  -> Map [CommandInfo] [OptionHelp]
selectSubtable cmds =
  Map.filterWithKey (\infos _ -> isParentCommand cmds infos)

isParentCommand :: [Text] -> [CommandInfo] -> Bool
isParentCommand cmds =
  and . zipWith (\cmd info -> cmd `elem` cmdNames info) cmds

-- | Render formatted help information for all commands and options
-- that exist underneath the current command context.
renderHelp
  :: ParseTree UnixScheme r
  -> [Token UnixScheme] -- ^ Context Stack
  -> Builder
renderHelp tree contexts =
  renderTables
  $ selectSubtable commandContext
  $ collectOptions tree
  where
    commandContext = reverse [s | UnixCommand s <- contexts]
