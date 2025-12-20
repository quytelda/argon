{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Text where

import           Control.Monad.Except
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB

import           StreamParser

class Render a where
  render :: a -> TLB.Builder

instance Render T.Text where
  render = TLB.fromText

instance Render Char where
  render = TLB.singleton

--------------------------------------------------------------------------------
-- Text Parser

data TextParser r = TextParser
  { parserHint :: Text
  , parserRun :: Text -> Except TLB.Builder r
  } deriving (Functor)

runTextParser :: TextParser r -> Text -> StreamParser tok r
runTextParser tp = liftExcept . parserRun tp

--------------------------------------------------------------------------------
-- Utility Functions

-- | Parse a 'Text' of the form "key=value" into ("key", "value"). If
-- the delimiter ('=') does not appear in the string, the result is
-- 'Nothing'.
keyEqualsValue :: Text -> Maybe (Text, Text)
keyEqualsValue s =
  case T.break (== '=') s of
    (key, T.uncons -> Just (_, value)) -> Just (key, value)
    _                                  -> Nothing
