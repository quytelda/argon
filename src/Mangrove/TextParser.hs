{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : Mangrove.TextParser
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Structures for parsing text input, along with some default parsers.
-}

module Mangrove.TextParser
  ( TextParser(..)
  , runTextParser
  , DefaultParser(..)
  ) where

import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Read         as TR

import           Mangrove.Text

-- | A @TextParser@ is the most basic client-defined parsing unit. It
-- parses textual data that is not otherwise part of the parsing
-- scheme into the actual results that will be combined and returned
-- once parsing completes.
data TextParser r = TextParser
  { parserHint :: !Text -- ^ A hint about the type of input this parser expects
  , parserRun  :: Text -> Either Text r -- ^ An actual parsing function
  } deriving (Functor)

-- | A more general function for running t'TextParser's.
runTextParser :: MonadError Builder m => TextParser r -> Text -> m r
runTextParser tp = liftEither . first TLB.fromText . parserRun tp

-- | A typeclass for types that have a convenient default
-- t'TextParser' implementation.
class DefaultParser r where
  -- | A reasonable default TextParser implementation.
  defaultParser :: TextParser r

exactly :: TR.Reader a -> Text -> Either Text a
exactly reader text =
  case reader text of
    Left err            -> throwError $ T.pack err
    Right (result, "")  -> pure result
    Right (_, leftover) -> throwError $ "unexpected input: " <> leftover

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

instance DefaultParser String where
  defaultParser = TextParser
    { parserHint = "STRING"
    , parserRun = pure . T.unpack
    }
