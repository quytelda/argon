{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module General (spec) where

import           Control.Applicative
import           Data.Either

import           Test.Hspec

import           Parser.Cli
import           ParseTree
import           Text

import           TestParsers

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
          `shouldBe` Left "--example option: unrecognized subargument: qwer"

  context "when no argument is expected" $ do
    context "when an argument is available" $ do
      it "doesn't consume the argument" $ do
        let result = parseArguments opt_example_unit ["--example", "qwer"]
        case result of
          Right ((), leftovers) -> render <$> leftovers `shouldBe` ["qwer"]
          Left err              -> expectationFailure (show err)

  context "when an argument is required" $ do
    it "renders with parameter hint" $ do
      render opt_example_param `shouldBe` "--example=STRING"
      render opt_e_param `shouldBe` "-e STRING"

    context "when no argument is provided" $ do
      it "fails to parse" $ do
        parseArguments opt_example_param ["--example"]
          `shouldBe` Left "--example option: expected: STRING"
    context "when an argument is provided" $ do
      it "the argument is consumed" $ do
        parseArguments opt_example_param ["--example", "qwer"]
          `shouldBe` Right ("qwer", [])

  context "when an argument is optional" $ do
    it "renders parameter hint in brackets" $ do
      render opt_example_param_optional `shouldBe` "--example=[STRING]"

    context "when no argument is provided" $ do
      it "yields a default value" $ do
        parseArguments opt_example_param_optional ["--example"]
          `shouldBe` Right ("asdf", [])
    context "when an argument is provided" $ do
      it "parses the argument" $ do
        parseArguments opt_example_param_optional ["--example", "qwer"]
          `shouldBe` Right ("qwer", [])

generalSpec :: Spec
generalSpec = do
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
        `shouldBe` Left "expected: STRING"

  context "when not all input can be consumed" $ do
    it "returns unconsumed arguments" $ do
      parseArguments param_text ["asdf", "qwer"]
        `shouldBe` Right ("asdf", [Argument "qwer"])

  context "when the first token is a bound argument" $ do
    it "fails without consuming any tokens" $ do
      runParseTree param_text [Bound "asdf"]
        `shouldBe` Left "unexpected subargument \"asdf\""

spec :: Spec
spec = do
  describe "General functionality" generalSpec
  describe "CLI Options" optionSpec
