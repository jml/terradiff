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

import qualified Data.Attoparsec.Text as A
import qualified JmlSvc
import qualified Options.Applicative as Opt

import qualified MassDriver.API as API
import MassDriver.Duration (Duration)
import qualified MassDriver.Duration as Duration
import qualified MassDriver.Poll as Poll
import qualified MassDriver.Terraform as Terraform

-- | Overall command-line configuration.
data Config
  = Config
  { serverConfig :: JmlSvc.Config
  , apiConfig :: API.APIConfig
  , terraformConfig :: Terraform.FlagConfig
  , pollInterval :: Duration
  }
  deriving (Eq, Show)

-- | Command-line parser for mass-driver.
options :: Opt.ParserInfo Config
options = Opt.info (Opt.helper <*> parser) description
  where
    parser = Config <$> JmlSvc.flags <*> API.flags <*> Terraform.flags <*> duration
    duration =
      Opt.option (Opt.eitherReader (A.parseOnly Duration.durationParser . toS))
      (fold
       [ Opt.long "poll-interval"
       , Opt.help "How frequently to run 'terraform plan'"
       ])
    description =
      fold
        [ Opt.fullDesc
        , Opt.progDesc "Terraform with style"
        , Opt.header "mass-driver - automatically apply Terraform configurations"
        ]

-- | Run the mass-driver API server.
run :: Config -> IO ()
run Config{serverConfig, apiConfig, terraformConfig, pollInterval} = do
  tfConfig <- Terraform.validateFlagConfig terraformConfig
  initResult <- Terraform.init tfConfig
  case initResult of
    (ExitFailure n, out, err) ->
      die (toS ("'terraform init' failed: " <> show n <> "\n" <>
                "output:\n" <> out <> "\n" <>
                "error:\n" <> err <> "\n"))
    (ExitSuccess, _, _) ->
      Poll.runWhilePolling (Terraform.diff tfConfig) (Duration.toDiffTime pollInterval)
        (JmlSvc.run "mass-driver" serverConfig . API.app apiConfig)

someFunc :: Int -> Int -> Int
someFunc x y = x + y

