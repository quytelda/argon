{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mangrove.StreamSpec (spec) where

import           Data.Text.Lazy.Builder
import           Test.Hspec

import           Mangrove
import           Mangrove.Scheme.Unix
import           Mangrove.Stream

data StreamResult r
  = SSuccess r
  | SEmpty
  | SFailure Builder
  | SRequest RequestType
  deriving (Eq, Show)

-- | Sink the results of a 'StreamParser' into a data type for easier inspection.
runStreamParser'
  :: SupportsResponse s
  => StreamParser s r
  -> StreamState s
  -> (StreamState s, StreamResult r)
runStreamParser' parser state =
  runStreamParser parser handler state
  where
    handler = StreamHandler
      { onSuccess = \s result -> (s, SSuccess result)
      , onEmpty = \s -> (s, SEmpty)
      , onFailure = \s err -> (s, SFailure err)
      , onRequest = OnRequest $ \s t -> (s, SRequest t)
      }

initState_empty :: StreamState s
initState_empty = StreamState [] [] False

initState_singleton :: StreamState s
initState_singleton = StreamState ["asdf"] [] False

spec :: Spec
spec = do
  describe "peek" $ do
    context "when the stream is empty" $ do
      let (finalState, result) = runStreamParser' peek (initState_empty @UnixScheme)
      it "returns empty" $ do
        result `shouldBe` SEmpty
      it "preserves the state" $ do
        initState_empty `shouldBe` finalState

    context "when the stream is not empty" $ do
      let (finalState, result) = runStreamParser' peek (initState_singleton @UnixScheme)
      it "gets the first item" $ do
        result `shouldBe` SSuccess "asdf"
      it "preserves the state" $ do
        initState_singleton `shouldBe` finalState

  describe "pop" $ do
    context "when the stream is empty" $ do
      let (finalState, result) = runStreamParser' pop (initState_empty @UnixScheme)
      it "returns empty" $ do
        result `shouldBe` SEmpty
      it "preserves the state" $ do
        initState_empty `shouldBe` finalState

    context "when the stream is not empty" $ do
      let (finalState, result) = runStreamParser' pop (initState_singleton @UnixScheme)
      it "gets the first item without replacement" $ do
        result `shouldBe` SSuccess "asdf"
        streamContent finalState `shouldBe` drop 1 (streamContent initState_singleton)
      it "preserves the context" $ do
        streamContext initState_singleton `shouldBe` streamContext finalState
