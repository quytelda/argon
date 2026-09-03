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
  ( -- * TextParser
    TextParser(..)
  , runTextParser

    -- * Parsers for Common Types
  , parseBool
  , parseInt
  , parseInteger
  , parseWord
  , parseChar
  , parseFloat
  , parseDouble
  , parseText
  , parseLazyText
  , parseLazyTextBuilder
  , parseString
  , parseFilePath
  , showsTextParser

    -- * Automatic Parser Selection
  , DefaultParser(..)
  ) where

import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
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

instance Show (TextParser r) where
  showsPrec p parser = showParen (p >= 11)
    $ showString "TextParser "
    . showString "{ parserHint = " . shows (parserHint parser)
    . showString ", parserRun = _"
    . showString "}"

-- | A nicer way to Show 'TextParser's is to use the parser's hint,
-- surrounded by angle brackets, e.g. @<INT>@.
showsTextParser :: TextParser a -> ShowS
showsTextParser TextParser{parserHint = hint} =
  showString "<"
  . showString (T.unpack hint)
  . showString ">"

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

-- | Parses a boolean value. This parser accepts @"true"@, @"false"@,
-- @"yes"@, or @"no"@ as input.
parseBool :: TextParser Bool
parseBool = TextParser
  { parserHint = "BOOL"
  , parserRun = parse
  }
  where
    parse "true"  = pure True
    parse "false" = pure False
    parse "yes"   = pure True
    parse "no"    = pure False
    parse _       = throwError "expected true|false|yes|no"

instance DefaultParser Bool where
  defaultParser = parseBool

-- | Parse a signed 'Int' value in base-10.
parseInt :: TextParser Int
parseInt = TextParser
  { parserHint = "INT"
  , parserRun = exactly TR.decimal
  }

instance DefaultParser Int where
  defaultParser = parseInt

-- | Parse a signed 'Integer' value in base-10.
parseInteger :: TextParser Integer
parseInteger = TextParser
  { parserHint = "INT"
  , parserRun = exactly TR.decimal
  }

instance DefaultParser Integer where
  defaultParser = parseInteger

-- | Parse an unsigned `Word` value in base-10.
parseWord :: TextParser Word
parseWord = TextParser
  { parserHint = "INT"
  , parserRun = exactly TR.decimal
  }

instance DefaultParser Word where
  defaultParser = parseWord

-- | Parse exactly one character. If the input is longer than 1 character, the parser fails.
parseChar :: TextParser Char
parseChar = TextParser
  { parserHint = "CHAR"
  , parserRun = parse
  }
  where
    parse (T.unpack -> [c]) = pure c
    parse _                 = throwError "input contains multiple characters"

instance DefaultParser Char where
  defaultParser = parseChar

-- | Parse a floating point value in base-10.
parseFloat :: TextParser Float
parseFloat = TextParser
  { parserHint = "FLOAT"
  , parserRun = exactly TR.rational
  }

instance DefaultParser Float where
  defaultParser = parseFloat

-- | Parse a double width value in base-10.
parseDouble :: TextParser Double
parseDouble = TextParser
  { parserHint = "DOUBLE"
  , parserRun = exactly TR.rational
  }

instance DefaultParser Double where
  defaultParser = parseDouble

-- | Parse a strict 'T.Text' value.
--
-- Since the input is already strict 'Text', this parser simply returns it for free.
parseText :: TextParser Text
parseText = TextParser
  { parserHint = "STRING"
  , parserRun = pure
  }

instance DefaultParser Text where
  defaultParser = parseText

-- | Parse a lazy 'TL.Text' value.
parseLazyText :: TextParser TL.Text
parseLazyText = TextParser
  { parserHint = "STRING"
  , parserRun = pure . TL.fromStrict
  }

instance DefaultParser TL.Text where
  defaultParser = parseLazyText

-- | Parse a lazy text 'TLB.Builder'.
parseLazyTextBuilder :: TextParser TLB.Builder
parseLazyTextBuilder = TextParser
  { parserHint = "STRING"
  , parserRun = pure . TLB.fromText
  }

instance DefaultParser TLB.Builder where
  defaultParser = parseLazyTextBuilder

-- | Parse a Haskell 'String' (i.e. @[Char]@) value.
parseString :: TextParser String
parseString = TextParser
  { parserHint = "STRING"
  , parserRun = pure . T.unpack
  }

instance DefaultParser String where
  defaultParser = parseString

-- | Parser for 'FilePath's.
--
-- This is the same as 'parseString' but with a more specialized
-- parser hint.
parseFilePath :: TextParser FilePath
parseFilePath = parseString { parserHint = "PATH" }
