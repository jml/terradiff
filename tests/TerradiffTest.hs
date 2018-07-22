-- | Tests for terradiff.
--
-- Copyright (c) 2018 Jonathan M. Lange
--
-- This file is part of terradiff.
--
-- terradiff is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- terradiff is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with terradiff. If not, see <https://www.gnu.org/licenses/>.
module TerradiffTest where

import Protolude

import Data.String (String)

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

import qualified Terradiff

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_terradiff :: Spec
spec_terradiff =
  describe "someFunc" $
    it "adds numbers" $ do
      let expected = 2
      let observed = Terradiff.someFunc 1 1
      observed `shouldBe` expected
