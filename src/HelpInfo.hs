{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module HelpInfo where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.Functor
import qualified Data.List              as List
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.List.NonEmpty     as NonEmpty
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

import           Parser
import           Parser.Cli
import           Parser.Sub
import           Parser.Text
import           ParseTree
import           Stream
import           Text

mwhen :: Monoid a => Bool -> a -> a
mwhen condition a = if condition then a else mempty

data OptionHelp = OptionHelp
  { ohShorts :: TL.Text
  , ohLongs  :: TL.Text
  , ohArg    :: TL.Text
  , ohDesc   :: TL.Text
  } deriving (Show)

mkOptionHelp :: OptionInfo -> ParseTree SubParser r -> OptionHelp
mkOptionHelp info subtree =
  OptionHelp
  { ohShorts = fmtFlagList shorts
  , ohLongs  = fmtFlagList longs
  , ohArg    = mwhen (valencyIs (> 0) subtree)
               $ TLB.toLazyText $ render subtree
  , ohDesc   = TL.fromStrict $ optHelp info
  }
  where
    (longs, shorts) = NonEmpty.partition (\case LongFlag {} -> True; _ -> False) $ optFlags info
    fmtFlagList = TLB.toLazyText . mconcat . List.intersperse ", " . fmap render

fmtOptionHelps :: [OptionHelp] -> Builder
fmtOptionHelps xs = foldMap (\x -> formatRow x <> "\n") xs
  where
    maxLengthBy f = maximum $ TL.length . f <$> xs
    col1width = maxLengthBy ohShorts
    col2width = maxLengthBy ohLongs
    col3width = maxLengthBy ohArg

    formatRow OptionHelp{..} =
      foldMap TLB.fromLazyText
      $ List.intersperse "  "
      [ TL.justifyLeft col1width ' ' ohShorts
      , TL.justifyLeft col2width ' ' ohLongs
      , TL.justifyLeft col3width ' ' ohArg
      , ohDesc
      ]
