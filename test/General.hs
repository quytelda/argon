{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module General (spec) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Test.Hspec

import           Argon
import           Parser.Cli
import           Parser.Sub
import           Parser.Text
import           ParseTree
import           Text

param_text :: ParseTree CliParser Text
param_text = defaultParameter

option_asdf :: ParseTree CliParser Text
option_asdf = option ["--asdf", "-a"] "" $ pure "qwer"

command_asdf :: ParseTree CliParser Text
command_asdf = command ["asdf"] "" $ pure "qwer"

spec :: Spec
spec = do
  context "when \"-\" is given as an argument" $ do
    it "parses the string \"-\"" $ do
      parseArguments param_text ["-"]
        `shouldBe` Right ("-", [])

  context "when \"--\" is present in the argument list" $ do
    it "treats subsequent arguments as free arguments" $ do
      parseArguments param_text ["--", "asdf"]
        `shouldBe` Right ("asdf", [])
    it "doesn't treat subsequent arguments as options" $ do
      parseArguments (option_asdf <|> param_text) ["--", "--asdf"]
        `shouldBe` Right ("--asdf", [])
    it "doesn't treat subsequent arguments as commands" $ do
      parseArguments (command_asdf <|> param_text) ["--", "asdf"]
        `shouldBe` Right ("asdf", [])

  context "when not enough input is provided" $ do
    it "fails to generate a result" $ do
      parseArguments param_text []
        `shouldSatisfy` isLeft

  context "when not all input can be consumed" $ do
    it "returns unconsumed arguments" $ do
      parseArguments param_text ["asdf", "qwer"]
        `shouldBe` Right ("asdf", [Argument "qwer"])

  context "when the first token is a bound argument" $ do
    it "fails without consuming any tokens" $ do
      runParseTree param_text [Bound "asdf"]
        `shouldBe` Left "unexpected subargument \"asdf\""
