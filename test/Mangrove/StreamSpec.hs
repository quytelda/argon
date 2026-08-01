{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mangrove.StreamSpec (spec) where

import           Control.Applicative
import           Data.Text.Lazy.Builder
import           Data.Void
import           Test.Hspec

import           Mangrove.Stream

data StreamResult r
  = SSuccess r
  | SEmpty
  | SFailure Builder
  | SHelpReq
  deriving (Eq, Show)

runStreamParser' :: StreamParser tok r -> StreamState tok -> (StreamState tok, StreamResult r)
runStreamParser' parser state =
  runStreamParser parser handler state
  where
    handler = StreamHandler
      { onSuccess = \s result -> (s, SSuccess result)
      , onEmpty = \s -> (s, SEmpty)
      , onFailure = \s err -> (s, SFailure err)
      , onHelpRequest = \s -> (s, SHelpReq)
      }

initState_empty :: StreamState tok
initState_empty = StreamState [] [] False

initState_singleton :: StreamState tok
initState_singleton = StreamState ["asdf"] [] False

spec :: Spec
spec = do
  describe "peek" $ do
    context "when the stream is empty" $ do
      let (finalState, result) = runStreamParser' peek (initState_empty @Void)
      it "returns empty" $ do
        result `shouldBe` SEmpty
      it "preserves the state" $ do
        initState_empty `shouldBe` finalState

    context "when the stream is not empty" $ do
      let (finalState, result) = runStreamParser' peek (initState_singleton @Void)
      it "gets the first item" $ do
        result `shouldBe` SSuccess "asdf"
      it "preserves the state" $ do
        initState_singleton `shouldBe` finalState

  describe "pop" $ do
    context "when the stream is empty" $ do
      let (finalState, result) = runStreamParser' pop (initState_empty @Void)
      it "returns empty" $ do
        result `shouldBe` SEmpty
      it "preserves the state" $ do
        initState_empty `shouldBe` finalState

    context "when the stream is not empty" $ do
      let (finalState, result) = runStreamParser' pop (initState_singleton @Void)
      it "gets the first item without replacement" $ do
        result `shouldBe` SSuccess "asdf"
        streamContent finalState `shouldBe` tail (streamContent initState_singleton)
      it "preserves the context" $ do
        streamContext initState_singleton `shouldBe` streamContext finalState
