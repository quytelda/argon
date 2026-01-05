module Main (main) where

import qualified Spec
import           Test.Hspec

import qualified General

main :: IO ()
main = hspec $ do
  describe "General" General.spec
  Spec.spec
