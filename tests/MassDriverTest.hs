-- | Tests for mass-driver.
--
-- Copyright (c) 2018 Jonathan M. Lange
--
-- This file is part of mass-driver.
--
-- mass-driver is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- mass-driver is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with mass-driver. If not, see <https://www.gnu.org/licenses/>.
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
