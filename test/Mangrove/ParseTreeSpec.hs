{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mangrove.ParseTreeSpec (spec) where

import           Control.Applicative
import           Data.Text             (Text)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       hiding (Result (..))

import           Mangrove
import           Mangrove.ParseTree
import           Mangrove.Scheme.Unix
import           Mangrove.Valency

import           Arbitrary
import           StructureEq
import           TestParsers

--------------------------------------------------------------------------------
-- Functor Laws

prop_fmapIdLaw :: UnixParser Int -> Bool
prop_fmapIdLaw tree = structEq tree (fmap id tree)

prop_fmapComLaw :: UnixParser Int -> Bool
prop_fmapComLaw tree =
  fmap (inc . dbl) tree
  `structEq`
  (fmap inc . fmap dbl) tree
  where
    inc = (1+)
    dbl = (2*)

--------------------------------------------------------------------------------
-- Applicative Laws

prop_applicativeIdLaw
  :: ParseTree UnixScheme Int
  -> ArgList
  -> Bool
prop_applicativeIdLaw tree (ArgList args) =
  result1 == result2
  where
    result1 = runHelpfulParser_ (pure id <*> tree) args
    result2 = runHelpfulParser_ tree args

prop_applicativeHomLaw
  :: Fun Int Int
  -> Int
  -> ArgList
  -> Bool
prop_applicativeHomLaw (Fn f) value (ArgList args) =
  result1 == result2
  where
    tree1 = pure f <*> pure value :: ParseTree UnixScheme Int
    tree2 = pure (f value) :: ParseTree UnixScheme Int
    result1 = runHelpfulParser_ tree1 args
    result2 = runHelpfulParser_ tree2 args

prop_applicativeIntLaw
  :: Fun (Int, Int) Int
  -> ParseTree UnixScheme Int
  -> Int
  -> ArgList
  -> Bool
prop_applicativeIntLaw (Fn2 f) tree n (ArgList args) =
  result1 == result2
  where
    u = fmap f tree
    result1 = runHelpfulParser_ (u <*> pure n) args
    result2 = runHelpfulParser_ (pure ($ n) <*> u) args

prop_applicativeComLaw
  :: Fun (Int, Int) Int
  -> Fun (Int, Int) Int
  -> ParseTree UnixScheme Int
  -> ParseTree UnixScheme Int
  -> ParseTree UnixScheme Int
  -> ArgList
  -> Bool
prop_applicativeComLaw (Fn2 f) (Fn2 g) t1 t2 w (ArgList args) =
  result1 == result2
  where
    u = fmap f t1
    v = fmap g t2
    tree1 = pure (.) <*> u <*> v <*> w
    tree2 = u <*> (v <*> w)
    result1 = runHelpfulParser_ tree1 args
    result2 = runHelpfulParser_ tree2 args

--------------------------------------------------------------------------------

prop_liftA2AddsValencies
  :: UnixParser Int
  -> UnixParser Int
  -> Bool
prop_liftA2AddsValencies l r =
  valency (liftA2 (+) l r) == liftA2 (+) (valency l) (valency r)

prop_liftA2CombinesResults
  :: Fun (Int, Int) Int
  -> UnixParser Int
  -> UnixParser Int
  -> ArgList
  -> Bool
prop_liftA2CombinesResults (Fn2 f) l r (ArgList args) =
  case (resultL, resultR, resultA) of
    (Success _ x, Success _ y, Success _ z) -> z == f x y
    _ -> resultA == resultL || resultA == resultR
  where
    resultL = runHelpfulParser_ l args
    resultR = runHelpfulParser_ r args
    resultA = runHelpfulParser_ (liftA2 f l r) args

spec :: Spec
spec = do
  describe "Functor Instance" $ do
    prop "satisfies identity law"
      prop_fmapIdLaw
    prop "satisfies composition law"
      prop_fmapComLaw

  describe "Applicative Instance" $ do
    prop "satisfies identity law"
      prop_applicativeIdLaw
    prop "satisfies homomorphism law"
      prop_applicativeHomLaw
    prop "satisfies interchange law"
      prop_applicativeIntLaw
    prop "satisfies composition law"
      prop_applicativeComLaw

  describe "pure" $ do
    it "resolves to the given value" $ do
      runHelpfulParser_ (ValueNode 'a' :: ParseTree UnixScheme Char) []
        `shouldBe` Success [] 'a'

  describe "liftA2" $ do
    it "combines two values" $ do
      runHelpfulParser_ (liftA2 (+) (pure 1) (pure 2) :: ParseTree UnixScheme Int) []
        `shouldBe` Success [] 3

      -- should be equivalent
      runHelpfulParser_ ((+) <$> pure 1 <*> pure 2 :: ParseTree UnixScheme Int) []
        `shouldBe` Success [] 3

    prop "combines results"
      prop_liftA2CombinesResults
    prop "adds valencies"
      prop_liftA2AddsValencies

  describe "empty" $ do
    it "doesn't resolve to any value" $ do
      runHelpfulParser_ (empty :: ParseTree UnixScheme Char) []
        `shouldBe` Failure "empty"

  describe "(<|>)" $ do
    context "when the left child is resolvable" $ do
      it "resolves as the left child" $ do
        runHelpfulParser_ (pure "asdf" <|> opt_e_param) []
          `shouldBe` Success [] "asdf"

        -- When the right child is also resolvable, it should be
        -- ignored.
        runHelpfulParser_ (pure "asdf" <|> pure "qwer" :: ParseTree UnixScheme Text) []
          `shouldBe` Success [] "asdf"

    context "when the left child is unresolvable" $ do
      it "resolves as the right child" $ do
        runHelpfulParser_ (opt_e_param <|> pure "asdf") []
          `shouldBe` Success [] "asdf"

    context "when one child is triggered" $ do
      it "prunes the other child" $ do
        runHelpfulParser_ (opt_e_unit <|> opt_f_unit) ["-e", "-f"]
          `shouldBe` Success ["-f"] ()
        runHelpfulParser_ (opt_e_unit <|> opt_f_unit) ["-f", "-e"]
          `shouldBe` Success ["-e"] ()

  describe "many" $ do
    it "parses multiple instances" $ do
      runHelpfulParser_ (many opt_e_param) ["-e", "asdf", "-e", "qwer", "-e", "zxcv"]
        `shouldBe` Success [] ["asdf", "qwer", "zxcv"]
    it "parses zero instances" $ do
      runHelpfulParser_ (many opt_e_param) ["blah"]
        `shouldBe` Success ["blah"] []

    it "handles compound trees" $ do
      let tree = (opt_f_unit *> opt_e_param) <|> opt_example_param
      runHelpfulParser_ (many tree) ["-f", "-e", "asdf", "--example", "qwer"]
        `shouldBe` Success [] ["asdf", "qwer"]

    it "doesn't swallow arguments" $ do
      runHelpfulParser_ (many $ opt_f_unit *> opt_e_param) ["-f", "-e", "asdf", "-f"]
        `shouldBe` Failure "expected: -e"
        -- Some attempts at implementing many/some resulted in
        -- arguments being silently swallowed if they were consumed by
        -- a parser inside a ManyNode which didn't receive enough
        -- input to resolve. In some cases this didn't occur until the
        -- second instance of the subtree was triggered. The expected
        -- behavior in this case is to fail with a message about what
        -- input was missing.

  describe "some" $ do
    it "parses multiple instances" $ do
      runHelpfulParser_ (some opt_e_param) ["-e", "asdf", "-e", "qwer", "-e", "zxcv"]
        `shouldBe` Success [] ["asdf", "qwer", "zxcv"]
    it "requires at least one instance" $ do
      runHelpfulParser_ (some opt_e_param) ["blah"]
        `shouldBe` Failure "unexpected blah"

    it "handles compound trees" $ do
      let tree = (opt_f_unit *> opt_e_param) <|> opt_example_param
      runHelpfulParser_ (some tree) ["-f", "-e", "asdf", "--example", "qwer"]
        `shouldBe` Success [] ["asdf", "qwer"]

    it "doesn't swallow arguments" $ do
      runHelpfulParser_ (some $ opt_f_unit *> opt_e_param) ["-f", "-e", "asdf", "-f"]
        `shouldBe` Failure "expected: -e"

  describe "optional" $ do
    it "parses exactly one instance" $ do
      runHelpfulParser_ (optional opt_e_param) ["-e", "asdf", "-e", "qwer", "-e", "zxcv"]
        `shouldBe` Success [ "-e", "qwer", "-e", "zxcv"] (Just "asdf")
    it "parses zero instances" $ do
      runHelpfulParser_ (optional opt_e_param) ["blah"]
        `shouldBe` Success ["blah"] Nothing
