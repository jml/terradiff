module MassDriverTest where

import Protolude

import Data.String (String)

import Hedgehog (Property, (===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

import qualified MassDriver

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_massDriver :: Spec
spec_massDriver =
  describe "someFunc" $
    it "adds numbers" $ do
      let expected = 2
      let observed = MassDriver.someFunc 1 1
      observed `shouldBe` expected

-- | 'someFunc' is commutative.
hprop_someFuncCommutative :: Property
hprop_someFuncCommutative =
  property $ do
    x <- forAll $ Gen.int Range.linearBounded
    y <- forAll $ Gen.int Range.linearBounded
    MassDriver.someFunc x y === MassDriver.someFunc y x
