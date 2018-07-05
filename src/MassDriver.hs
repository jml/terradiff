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
  , run
  ) where

import Protolude

import qualified JmlSvc
import qualified Options.Applicative as Opt

import qualified MassDriver.API as API
import qualified MassDriver.Terraform as Terraform

-- | Overall command-line configuration.
data Config
  = Config
  { serverConfig :: JmlSvc.Config
  , apiConfig :: API.APIConfig
  , terraformConfig :: Terraform.FlagConfig
  }
  deriving (Eq, Show)

-- | Command-line parser for mass-driver.
options :: Opt.ParserInfo Config
options = Opt.info (Opt.helper <*> parser) description
  where
    parser = Config <$> JmlSvc.flags <*> API.flags <*> Terraform.flags
    description =
      fold
        [ Opt.fullDesc
        , Opt.progDesc "Terraform with style"
        , Opt.header "mass-driver - automatically apply Terraform configurations"
        ]

-- | Run the mass-driver API server.
run :: MonadIO io => Config -> io ()
run Config{serverConfig, apiConfig, terraformConfig} = do
  tfConfig <- Terraform.validateFlagConfig terraformConfig
  JmlSvc.run "mass-driver" serverConfig (API.app apiConfig tfConfig)

someFunc :: Int -> Int -> Int
someFunc x y = x + y
