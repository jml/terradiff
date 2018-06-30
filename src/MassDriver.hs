-- | Terraforming with style.
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
module MassDriver
  ( Config
  , options
  , someFunc
  ) where

import Protolude

import qualified Options.Applicative as Opt

-- | Overall command-line configuration.
data Config = Config deriving (Eq, Show)

-- | Command-line parser for mass-driver.
options :: Opt.ParserInfo Config
options = Opt.info (Opt.helper <*> parser) description
  where
    parser = pure Config

    description =
      fold
        [ Opt.fullDesc
        , Opt.progDesc "short description of your program"
        , Opt.header "mass-driver - short description of your program"
        ]

someFunc :: Int -> Int -> Int
someFunc x y = x + y
