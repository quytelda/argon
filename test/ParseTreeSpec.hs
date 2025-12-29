{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseTreeSpec (spec) where

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

opt_example_unit :: ParseTree CliParser ()
opt_example_unit = option [LongFlag "example"] "" $ pure ()

opt_mlem_unit :: ParseTree CliParser ()
opt_mlem_unit = option [LongFlag "mlem"] "" $ pure ()

opt_e_unit :: ParseTree CliParser ()
opt_e_unit = option [ShortFlag 'e'] "" $ pure ()

opt_f_unit :: ParseTree CliParser ()
opt_f_unit = option [ShortFlag 'f'] "" $ pure ()

opt_example_param :: ParseTree CliParser Text
opt_example_param = option [LongFlag "example"] "" defaultParameter

opt_example_switch :: ParseTree CliParser Bool
opt_example_switch = switch [LongFlag "example"] ""

opt_example_param_optional :: ParseTree CliParser Text
opt_example_param_optional =
  option [LongFlag "example"] "" (defaultParameter <|> pure "asdf")

optionSpec :: Spec
optionSpec = do
  it "parses long options" $ do
    parseArguments opt_example_unit ["--example"]
      `shouldBe` Right ((), [])
  it "parses short options" $ do
    parseArguments opt_e_unit ["-e"]
      `shouldBe` Right ((), [])
  it "parses short option groups" $ do
    parseArguments (opt_e_unit *> opt_f_unit) ["-ef"]
      `shouldBe` Right ((), [])

  it "parses options in any order" $ do
    parseArguments (opt_e_unit *> opt_f_unit) ["-ef"]
      `shouldBe` Right ((), [])
    parseArguments (opt_e_unit *> opt_f_unit) ["-fe"]
      `shouldBe` Right ((), [])

  describe "switches" $ do
    context "when switch is present" $ do
      it "yields True" $ do
        parseArguments opt_example_switch ["--example"]
          `shouldBe` Right (True, [])
    context "when switch is absent" $ do
      it "yields False" $ do
        parseArguments opt_example_switch []
          `shouldBe` Right (False, [])

  context "when a bound argument is provided" $ do
    context "when an argument is expected" $ do
      it "parses the argument" $ do
        parseArguments opt_example_param ["--example=qwer"]
          `shouldBe` Right ("qwer", [])
    context "when no argument is expected" $ do
      it "parsing fails" $ do
        parseArguments opt_example_unit ["--example=qwer"]
          `shouldSatisfy` isLeft

  context "when no argument is expected" $ do
    context "when an argument is available" $ do
      it "doesn't consume the argument" $ do
        let result = parseArguments opt_example_unit ["--example", "qwer"]
        case result of
          Right ((), leftovers) -> render <$> leftovers `shouldBe` ["qwer"]
          Left err              -> expectationFailure (show err)

  context "when an argument is required" $ do
    context "when no argument is provided" $ do
      it "fails to parse" $ do
        parseArguments opt_example_param ["--example"]
          `shouldSatisfy` isLeft
    context "when an argument is provided" $ do
      it "the argument is consumed" $ do
        parseArguments opt_example_param ["--example", "qwer"]
          `shouldBe` Right ("qwer", [])

  context "when an argument is optional" $ do
    context "when no argument is provided" $ do
      it "yields a default value" $ do
        parseArguments opt_example_param_optional ["--example"]
          `shouldBe` Right ("asdf", [])
    context "when an argument is provided" $ do
      it "parses the argument" $ do
        parseArguments opt_example_param_optional ["--example", "qwer"]
          `shouldBe` Right ("qwer", [])    

spec :: Spec
spec = do
  describe "CLI Options" optionSpec
