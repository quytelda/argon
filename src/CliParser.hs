{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module CliParser where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.Functor
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.List.NonEmpty     as NonEmpty
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB

import           Parser
import           ParseTree
import           StreamParser
import           SubParser
import           Text
import           TextParser

--------------------------------------------------------------------------------
-- User Interface Descriptions

data Flag
  = LongFlag Text
  | ShortFlag Char
  deriving (Eq, Show)

instance IsString Flag where
  fromString ('-':'-':name)
    | not (null name) && all isAlphaNum name = LongFlag $ T.pack name
  fromString ('-':c:[])
    | isAlphaNum c = ShortFlag c
  fromString s = error $ "not a valid flag: " <> s

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

-- | Parsers for top-level CLI arguments such as commands and options.
data CliParser r
  = CliParameter (TextParser r)
  | CliOption OptionInfo (ParseTree SubParser r)
  | CliCommand CommandInfo (ParseTree CliParser r)
  deriving (Functor)

instance Render (CliParser r) where
  render = renderParser

instance HasValency CliParser where
  valency (CliParameter _) = Just 1
  valency (CliOption _ subtree) = case valency subtree of
                                    Just n | n <= 0 -> Just 1
                                    _               -> Just 2
  valency (CliCommand _ subtree) = (+1) <$> valency subtree

instance Resolve CliParser where
  resolve (CliParameter (TextParser hint _)) =
    throwError $ "expected " <> TLB.fromText hint
  resolve (CliOption info _) =
    throwError $ "expected option " <> render (optHead info)
  resolve (CliCommand info _) =
    throwError $ "expected command " <> render (cmdHead info)

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

  sepProd _ = " "
  sepSum _ = " | "
  renderParser (CliParameter tp) = render $ parserHint tp
  renderParser (CliOption info subtree) = render (optHead info)
                                          <> if valencyIs (> 0) subtree
                                             then "=" <> render subtree
                                             else mempty
  renderParser (CliCommand info subtree) = "{" <> render (cmdHead info) <> " "
                                           <> render subtree <> "}"

  accepts (CliParameter _) (Argument _)      = True
  accepts (CliParameter _) (Escaped _)       = True
  accepts (CliOption info _) (LongOption s)  = LongFlag s `elem` optFlags info
  accepts (CliOption info _) (ShortOption c) = ShortFlag c `elem` optFlags info
  accepts (CliCommand info _) (Argument s)   = s `elem` cmdNames info
  accepts _ _                                = False

  feedParser (CliParameter tp) = do
    text <- peek >>= \case
      Argument s -> pure s
      Escaped s  -> pure s
      _          -> empty

    withContext (render (parserHint tp) <> " parameter") $
      pop *> runTextParser tp text
  feedParser parser@(CliOption _ subtree) = do
    next <- peek
    guard $ parser `accepts` next
    void $ popMaybe

    withContext (render next <> " option") $ do
      -- Collect arguments for the subparser's stream from the next
      -- argument in the parent stream.
      let asList s = if valencyIs (> 1) subtree
                     then T.split (== ',') s
                     else [s]

      args <- peekMaybe <&> \case
        Just (Bound s)    -> asList s
        Just (Argument s) -> asList s
        _                 -> []

      -- Evaluate the subparser in a new stream context.
      (result, leftovers) <- runParseTree subtree args

      -- If the subparser consumed its input, we can safely remove it
      -- the from the parent stream. However, we cannot remove partially
      -- consumed input, so in that case we throw an error.
      when (length args /= length leftovers) $
        pop *> mapM_ (\arg -> throwError $ "unrecognized subargument: " <> render arg) leftovers

      -- Ensure we are not leaving an unconsumed bound argument at the
      -- head of the stream.
      peekMaybe >>= \case
        Just (Bound s) -> throwError $ "unrecognized subargument: " <> render s
        _ -> pure ()

      pure result
  feedParser parser@(CliCommand _ subtree) = do
    next <- peek
    guard $ parser `accepts` next

    withContext (render next <> " command") $
      pop
      *> satiate subtree
      >>= liftEither . resolve
