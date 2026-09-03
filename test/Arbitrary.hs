{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Arbitrary
  ( ArgList(..)
  , Name(..)
  ) where

import           Data.Char
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen       (Gen (..))
import           Test.QuickCheck.Instances ()

import           Mangrove
import           Mangrove.ParseTree
import qualified Mangrove.Scheme.Sub       as Sub
import           Mangrove.Scheme.Unix
import qualified Mangrove.Scheme.Unix      as Unix
import           Mangrove.Stream
import           Mangrove.Text
import           Mangrove.TextParser
import           Mangrove.Unix

--------------------------------------------------------------------------------
-- Arbitrary Name Generator

randomNameChar :: RandomGen g => g -> (Char, g)
randomNameChar gen = (chr (n + offset), gen')
  where
    (n, gen') = uniformR (0, 62) gen
    offset
      | n >= 0  && n < 10 = 48
      | n >= 10 && n < 36 = 55
      | n >= 36 && n < 62 = 61
      -- The only remaining case is n == 62.
      | otherwise = 33

randomNameText :: RandomGen g => g -> Int -> Text
randomNameText gen n = T.unfoldrN n (Just . randomNameChar) gen

genNameChar :: Gen Char
genNameChar = MkGen $ const . fst . randomNameChar

genNameText :: Gen Text
genNameText = MkGen randomNameText `suchThat` (not . T.null)

-- | newtype wrapper for 'Text' that holds results from 'genNameText'
newtype Name = Name { getName :: Text }
  deriving (Eq, Show)

getNames :: Functor f => f Name -> f Text
getNames = fmap getName

instance Arbitrary Name where
  arbitrary = Name <$> genNameText

--------------------------------------------------------------------------------
-- Generic ParseTrees

genParser :: Scheme s => Gen (s Int) -> Gen (ParseTree s Int)
genParser genScheme = sized $ \n -> oneof $
  if n <= 0
  then [ pure EmptyNode, ValueNode <$> arbitrary ]
  else [ pure EmptyNode
       , ValueNode <$> arbitrary
       , ParseNode <$> genScheme
       , ProdNode <$> arbitrary @(Int -> Int -> Int)
                  <*> genParser genScheme
                  <*> genParser genScheme
       , SumNode <$> genParser genScheme <*> genParser genScheme
         -- ManyNode can only give us a `UnixParser [Int]`, so we have
         -- to wrap it in order to make the types match.
       , (fmap . fmap) sum $ ManyNode <$> arbitrary <*> genParser genScheme
       ]

--------------------------------------------------------------------------------
-- SubScheme Parsers

instance Arbitrary (Token SubScheme) where
  arbitrary =
    oneof [ SubAssoc <$> genNameText <*> arbitrary
          , SubArgument <$> arbitrary
          ]

genSubScheme :: Gen (SubScheme Int)
genSubScheme =
  oneof [ pure $ Sub.Parameter defaultParser
        , flip Sub.Option defaultParser <$> arbitrary
        ]

instance Arbitrary (ParseTree SubScheme Int) where
  arbitrary = genParser genSubScheme

--------------------------------------------------------------------------------
-- UnixScheme Parsers

instance Arbitrary Flag where
  arbitrary =
    oneof [ LongFlag <$> genNameText
          , ShortFlag <$> genNameChar
          ]

instance Arbitrary Unix.OptionInfo where
  arbitrary = OptionInfo <$> arbitrary <*> arbitrary

instance Arbitrary Unix.CommandInfo where
  arbitrary = CommandInfo <$> fmap getNames arbitrary <*> arbitrary

instance Arbitrary (Token UnixScheme) where
  arbitrary =
    oneof [ UnixArgument <$> arbitrary
          , UnixCommand <$> genNameText
          , UnixOption <$> arbitrary <*> arbitrary
          ]

instance Arbitrary RequestType where
  arbitrary = elements [HelpRequest, VersionRequest]

genUnixScheme :: Gen (UnixScheme Int)
genUnixScheme =
  oneof [ pure $ Unix.Parameter defaultParser
        , Unix.Option <$> arbitrary <*> arbitrary
        , Unix.Command <$> arbitrary <*> arbitrary
        , Unix.RequestOption <$> arbitrary <*> arbitrary
        ]

instance Arbitrary (ParseTree UnixScheme Int) where
  arbitrary = genParser genUnixScheme

--------------------------------------------------------------------------------
-- StreamParsers

genUnixArgument :: Gen Text
genUnixArgument = renderText <$> arbitrary @(Token UnixScheme)

genUnixArgs :: Gen [Text]
genUnixArgs = sized $ \n -> vectorOf n genUnixArgument

newtype ArgList = ArgList { getArgs :: [Text] }
  deriving (Show)

instance Arbitrary ArgList where
  arbitrary = ArgList <$> genUnixArgs

instance Arbitrary (StreamState UnixScheme) where
  arbitrary = StreamState <$> genUnixArgs <*> arbitrary <*> arbitrary
