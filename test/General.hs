{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module General (spec) where

import           Control.Applicative

import           Test.Hspec

import           Mangrove.ArgumentParser
import           Mangrove.Text

import           TestParsers

optionSpec :: Spec
optionSpec = do
  it "parses long options" $ do
    runHelpfulParser_ opt_example_unit ["--example"]
      `shouldBe` Success [] ()
  it "parses short options" $ do
    runHelpfulParser_ opt_e_unit ["-e"]
      `shouldBe` Success [] ()

  it "parses options in any order" $ do
    runHelpfulParser_ (opt_e_unit *> opt_f_unit) ["-e", "-f"]
      `shouldBe` Success [] ()
    runHelpfulParser_ (opt_e_unit *> opt_f_unit) ["-f", "-e"]
      `shouldBe` Success [] ()

  describe "switches" $ do
    context "when switch is present" $ do
      it "yields True" $ do
        runHelpfulParser_ opt_example_switch ["--example"]
          `shouldBe` Success [] True
    context "when switch is absent" $ do
      it "yields False" $ do
        runHelpfulParser_ opt_example_switch []
          `shouldBe` Success [] False

  context "when a bound argument is provided" $ do
    context "when an argument is expected" $ do
      it "parses the argument" $ do
        runHelpfulParser_ opt_example_param ["--example=qwer"]
          `shouldBe` Success [] "qwer"
        runHelpfulParser_ opt_e_param ["-eqwer"]
          `shouldBe` Success [] "qwer"
    context "when no argument is expected" $ do
      it "parsing fails" $ do
        runHelpfulParser_ opt_example_unit ["--example=qwer"]
          `shouldBe` Failure "--example=qwer: unrecognized subargument: qwer"
        runHelpfulParser_ opt_e_unit ["-eqwer"]
          `shouldBe` Failure "-eqwer: unrecognized subargument: qwer"

  context "when no argument is expected" $ do
    context "when an argument is available" $ do
      it "doesn't consume the argument" $ do
        runHelpfulParser_ opt_example_unit ["--example", "qwer"]
          `shouldBe` Success ["qwer"] ()

  context "when an argument is required" $ do
    it "renders with parameter hint" $ do
      render opt_example_param `shouldBe` "--example=STRING"
      render opt_e_param `shouldBe` "-eSTRING"

    context "when no argument is provided" $ do
      it "fails to parse" $ do
        runHelpfulParser_ opt_example_param ["--example"]
          `shouldBe` Failure "--example: expected: STRING"
    context "when an argument is provided" $ do
      it "the argument is consumed" $ do
        runHelpfulParser_ opt_example_param ["--example", "qwer"]
          `shouldBe` Success [] "qwer"

  context "when an argument is optional" $ do
    it "renders parameter hint in brackets" $ do
      render opt_example_param_optional `shouldBe` "--example=[STRING]"

    context "when no argument is provided" $ do
      it "yields a default value" $ do
        runHelpfulParser_ opt_example_param_optional ["--example"]
          `shouldBe` Success [] "asdf"
      it "does not consume subsequent options" $ do
        runHelpfulParser_ opt_example_param_optional ["--example", "--option"]
          `shouldBe` Success ["--option"] "asdf"
    context "when an argument is provided" $ do
      it "parses the argument" $ do
        runHelpfulParser_ opt_example_param_optional ["--example", "qwer"]
          `shouldBe` Success [] "qwer"

  describe "compound options" $ do
    context "when the subtree accepts multiple arguments" $ do
      it "splits the input by delimiter" $ do
        runHelpfulParser_ opt_example_pair ["--example", "1,3"]
          `shouldBe` Success [] (1,3)
    context "when the subtree can't accept multiple argument" $ do
      it "doesn't split the input by delimiter" $ do
        runHelpfulParser_ opt_example_param ["--example", "1,3"]
          `shouldBe` Success [] "1,3"
        runHelpfulParser_ opt_example_param_optional ["--example", "1,3"]
          `shouldBe` Success [] "1,3"

    context "when the subtree accepts suboptions" $ do
      it "parses key=value pairs" $ do
        runHelpfulParser_ opt_example_subopt ["--example", "value=asdf"]
          `shouldBe` Success [] "asdf"
        runHelpfulParser_ opt_example_subopt ["--example=value=asdf"]
          `shouldBe` Success [] "asdf"
    context "when the subtree can't accept suboptions" $ do
      it "doesn't parse key=value pairs" $ do
        runHelpfulParser_ opt_example_param ["--example", "value=asdf"]
          `shouldBe` Success [] "value=asdf"
        runHelpfulParser_ opt_example_param ["--example=value=asdf"]
          `shouldBe` Success [] "value=asdf"

  describe "help options" $ do
    let progInfo = ProgramInfo "example" "description"
        isHelpResult (Help _) = True
        isHelpResult _        = False

    context "when a help option is present" $ do
      it "requests help" $ do
        runHelpfulParser progInfo (withHelp opt_example_unit) ["--help"]
          `shouldSatisfy` isHelpResult
      it "works for subcommands" $ do
        runHelpfulParser progInfo (withHelp cmd_example_tree) ["example", "--help"]
          `shouldSatisfy` isHelpResult
        runHelpfulParser progInfo (withHelp cmd_example_tree) ["example", "asdf", "--help"]
          `shouldSatisfy` isHelpResult

    context "when a help option is absent" $ do
      it "doesn't request help" $ do
        runHelpfulParser_ (withHelp opt_example_unit) ["--example"]
          `shouldBe` Success [] ()
        runHelpfulParser_ (withHelp opt_example_unit) []
          `shouldBe` Failure "expected: --help or --example"
      it "isn't activated by escaped options" $ do
        runHelpfulParser_ (withHelp opt_example_unit) ["--", "--help"]
          `shouldBe` Failure "unexpected --help"

generalSpec :: Spec
generalSpec = do
  context "when \"-\" is given as an argument" $ do
    it "parses the string \"-\"" $ do
      runHelpfulParser_ param_text ["-"]
        `shouldBe` Success [] "-"

  context "when \"--\" is present in the argument list" $ do
    it "treats subsequent arguments as free arguments" $ do
      runHelpfulParser_ param_text ["--", "asdf"]
        `shouldBe` Success [] "asdf"
    it "doesn't treat subsequent arguments as options" $ do
      runHelpfulParser_ (option_asdf <|> param_text) ["--", "--asdf"]
        `shouldBe` Success [] "--asdf"
    it "doesn't treat subsequent arguments as commands" $ do
      runHelpfulParser_ (command_asdf <|> param_text) ["--", "asdf"]
        `shouldBe` Success [] "asdf"

  context "when not enough input is provided" $ do
    it "fails to generate a result" $ do
      runHelpfulParser_ param_text []
        `shouldBe` Failure "expected: STRING"

  context "when not all input can be consumed" $ do
    it "returns unconsumed arguments" $ do
      runHelpfulParser_ param_text ["asdf", "qwer"]
        `shouldBe` Success ["qwer"] "asdf"

spec :: Spec
spec = do
  describe "General functionality" generalSpec
  describe "CLI Options" optionSpec
