{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Text
  ( -- * Text Rendering
    Render(..)

    -- * Helpers
  , keyEqualsValue

    -- * Re-exports
  , Builder
  ) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB

-- | A class for things that can be rendered to a text 'Builder'.
class Render a where
  render :: a -> Builder

instance Render Builder where
  render = id

instance Render T.Text where
  render = TLB.fromText

instance Render Char where
  render = TLB.singleton

instance Render String where
  render = TLB.fromString

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
