module LibTest where

import Protolude

import Data.String (String)

import Hedgehog (Property, (===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

import qualified Lib

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_lib :: Spec
spec_lib =
  describe "someFunc" $
    it "adds numbers" $ do
      let expected = 2
      let observed = Lib.someFunc 1 1
      observed `shouldBe` expected

-- | 'someFunc' is commutative.
hprop_someFuncCommutative :: Property
hprop_someFuncCommutative =
  property $ do
    x <- forAll $ Gen.int Range.linearBounded
    y <- forAll $ Gen.int Range.linearBounded
    Lib.someFunc x y === Lib.someFunc y x
