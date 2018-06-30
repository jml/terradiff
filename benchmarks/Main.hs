-- | Benchmarks for mass-driver.
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
module Main (main) where

import Protolude

-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

main :: IO ()
main = defaultMain [bench "const" (whnf const ())]
