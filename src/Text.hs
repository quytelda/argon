{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
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
import qualified Data.Text.Read         as TR

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
  , parserRun  :: Text -> Except TLB.Builder r
  } deriving (Functor)

runTextParser :: TextParser r -> Text -> StreamParser tok r
runTextParser tp = liftExcept . parserRun tp

class DefaultParser r where
  defaultParser :: TextParser r

exactly :: MonadError TLB.Builder m => TR.Reader a -> Text -> m a
exactly reader text =
  case reader text of
    Left err            -> throwError $ TLB.fromString err
    Right (result, "")  -> pure result
    Right (_, leftover) -> throwError $ "unexpected input: " <> render leftover

instance DefaultParser Bool where
  defaultParser = TextParser
    { parserHint = "BOOL"
    , parserRun = parse
    }
    where
      parse "true"  = pure True
      parse "false" = pure False
      parse "yes"   = pure True
      parse "no"    = pure False
      parse _       = throwError "expected true|false|yes|no"

instance DefaultParser Int where
  defaultParser = TextParser
    { parserHint = "INT"
    , parserRun = exactly TR.decimal
    }

instance DefaultParser Integer where
  defaultParser = TextParser
    { parserHint = "INT"
    , parserRun = exactly TR.decimal
    }

instance DefaultParser Word where
  defaultParser = TextParser
    { parserHint = "INT"
    , parserRun = exactly TR.decimal
    }

instance DefaultParser Char where
  defaultParser = TextParser
    { parserHint = "CHAR"
    , parserRun = parse
    }
    where
      parse (T.unpack -> [c]) = pure c
      parse _                 = throwError "input contains multiple characters"

instance DefaultParser Float where
  defaultParser = TextParser
    { parserHint = "FLOAT"
    , parserRun = exactly TR.rational
    }

instance DefaultParser Double where
  defaultParser = TextParser
    { parserHint = "DOUBLE"
    , parserRun = exactly TR.rational
    }

instance DefaultParser Text where
  defaultParser = TextParser
    { parserHint = "STRING"
    , parserRun = pure
    }

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
