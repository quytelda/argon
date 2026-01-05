module Main (main) where

import qualified Spec
import           Test.Hspec

import qualified General

main :: IO ()
main = hspec $ do
  General.spec
  Spec.spec
