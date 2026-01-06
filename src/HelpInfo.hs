{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module HelpInfo
  ( renderHelpInfo
  ) where

import qualified Data.List.NonEmpty     as NonEmpty
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

import           Parser
import           Parser.Cli
import           Parser.Sub
import           ParseTree
import           Text

data OptionHelp = OptionHelp
  { ohShorts :: TL.Text
  , ohLongs  :: TL.Text
  , ohArg    :: TL.Text
  , ohDesc   :: TL.Text
  } deriving (Show)

-- | Like `Control.Monad.when` but for 'Monoid' instead of
-- 'Alternative'.
mwhen :: Monoid a => Bool -> a -> a
mwhen condition a = if condition then a else mempty

mkOptionHelp :: OptionInfo -> ParseTree SubParser r -> OptionHelp
mkOptionHelp OptionInfo{..} subtree =
  OptionHelp
  { ohShorts = fmtFlagList shorts
  , ohLongs  = fmtFlagList longs
  , ohArg    = mwhen (valencyIs (> 0) subtree)
               $ renderLazyText subtree
  , ohDesc   = TL.fromStrict optHelp
  }
  where
    isLongFlag LongFlag{} = True
    isLongFlag _          = False
    (longs, shorts) = NonEmpty.partition isLongFlag optFlags
    fmtFlagList = TL.intercalate ", " . fmap renderLazyText

collectOptions :: ParseTree CliParser r -> Map [CommandInfo] [OptionHelp]
collectOptions tree = go tree mempty
  where
    go :: ParseTree CliParser r
       -> Map [CommandInfo] [OptionHelp]
       -> Map [CommandInfo] [OptionHelp]
    go (ParseNode (CliOption info subtree)) =
      Map.insertWith (<>) [] [mkOptionHelp info subtree]
    go (ParseNode (CliCommand info subtree)) =
      Map.union $ Map.mapKeys (info :) $ collectOptions subtree
    go (ProdNode _ l r) = go r . go l
    go (SumNode l r)    = go r . go l
    go (ManyNode p)     = go p
    go _                = id

fmtOptionTable :: [OptionHelp] -> Builder
fmtOptionTable xs = foldMap formatRow xs
  where
    maxLengthBy f = maximum $ TL.length . f <$> xs
    col1width = maxLengthBy ohShorts
    col2width = maxLengthBy ohLongs
    col3width = maxLengthBy ohArg

    formatRow OptionHelp{..} =
      TLB.fromLazyText
      $ TL.intercalate "  "
      [ TL.justifyLeft col1width ' ' ohShorts
      , TL.justifyLeft col2width ' ' ohLongs
      , TL.justifyLeft col3width ' ' ohArg
      , ohDesc
      , "\n"
      ]

fmtHeader :: [CommandInfo] -> Builder
fmtHeader [] = mempty
fmtHeader cmds@(info : _) =
  fmtCommand cmds
  <> " command: "
  <> render (cmdHelp info)
  <> "\n"
  where
    quote m = "\"" <> m <> "\""
    fmtCommand = quote . render . T.unwords . fmap cmdHead . reverse

fmtAllSections :: Map [CommandInfo] [OptionHelp] -> Builder
fmtAllSections =
  Map.foldlWithKey
  (\acc cmds ohs ->
      acc
      <> "\n"
      <> fmtHeader cmds
      <> fmtOptionTable ohs
  ) mempty

renderHelpInfo :: Text -> Text -> ParseTree CliParser r -> Builder
renderHelpInfo name description tree =
  "Usage: " <> render name <> " " <> render tree <> "\n\n"
  <> render description <> "\n"
  <> fmtAllSections (collectOptions tree)
