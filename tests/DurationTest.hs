-- | Tests for Duration
module DurationTest where

import Protolude

import Data.String (String)

import Hedgehog (Property, (===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

import qualified MassDriver.Duration as Duration

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_formatDuration :: Spec
spec_formatDuration =
  describe "formatDuration" $ do
    it "formats 0" $
      Duration.formatDuration 0 `shouldBe` "0s"
    it "formats whole seconds" $
      Duration.formatDuration ((4 :: Double) `Duration.mul` Duration.second) `shouldBe` "4s"
    it "formats microseconds" $
      Duration.formatDuration 1000 `shouldBe` "1us"
    it "formats fractional microseconds" $
      Duration.formatDuration 1003 `shouldBe` "1.003us"
    it "formats minutes" $
      Duration.formatDuration 60000000000 `shouldBe` "1m0s"

-- | 'parseDuration' and 'formatDuration' roundrip.
hprop_parsingRoundtrips :: Property
hprop_parsingRoundtrips =
  property $ do
    x <- forAll $ Duration.Duration <$> Gen.integral Range.linearBounded
    Duration.parseDuration (Duration.formatDuration x) === Just x

-- | Some worked examples of 'parseDuration'.
spec_parseDuration :: Spec
spec_parseDuration =
  describe "parseDuration parses" $ do
    it "0s" $
      Duration.parseDuration "0s" `shouldBe` Just 0
    it "1us" $
      Duration.parseDuration "1us" `shouldBe` Just 1000
    it "1.003us" $
      Duration.parseDuration "1.003us" `shouldBe` Just 1003
    it "1m0s" $ do
      Duration.parseDuration "1m" `shouldBe` Just 60000000000
      Duration.parseDuration "1m0s" `shouldBe` Just 60000000000

-- | Build a 'Duration' from units and values.
spec_mul :: Spec
spec_mul =
  describe "mul" $
    it "allows fractional multipliers" $
      (1.003 :: Double) `Duration.mul` Duration.microsecond `shouldBe` 1003
