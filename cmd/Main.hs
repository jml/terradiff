-- | Run the mass-driver API server
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

import qualified Options.Applicative as Opt

import qualified MassDriver

main :: IO ()
main = do
  config <- Opt.execParser MassDriver.options
  let result = MassDriver.someFunc 2 3
  putText $ "Config = " <> show config
  putText $ "Result = " <> show result
