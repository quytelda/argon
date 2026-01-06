{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParsers where

import           Control.Applicative
import           Data.Text           (Text)

import           Test.Hspec

import           Argon
import           Parser.Cli
import           ParseTree

opt_example_unit :: ParseTree CliParser ()
opt_example_unit = option [LongFlag "example"] "" $ pure ()

opt_e_unit :: ParseTree CliParser ()
opt_e_unit = option [ShortFlag 'e'] "" $ pure ()

opt_e_param :: ParseTree CliParser Text
opt_e_param = option [ShortFlag 'e'] "" defaultParameter

opt_f_unit :: ParseTree CliParser ()
opt_f_unit = option [ShortFlag 'f'] "" $ pure ()

opt_example_param :: ParseTree CliParser Text
opt_example_param = option [LongFlag "example"] "" defaultParameter

opt_example_switch :: ParseTree CliParser Bool
opt_example_switch = switch [LongFlag "example"] ""

opt_example_param_optional :: ParseTree CliParser Text
opt_example_param_optional =
  option [LongFlag "example"] "" (defaultParameter <|> pure "asdf")

param_text :: ParseTree CliParser Text
param_text = defaultParameter

option_asdf :: ParseTree CliParser Text
option_asdf = option ["--asdf", "-a"] "" $ pure "qwer"

command_asdf :: ParseTree CliParser Text
command_asdf = command ["asdf"] "" $ pure "qwer"

testParser :: (Eq r, Show r) => ParseTree CliParser r -> [Text] -> r -> Expectation
testParser tree args expect = parseArguments tree args `shouldBe` Right (expect, [])
