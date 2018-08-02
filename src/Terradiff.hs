-- | Terraforming with style.
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
module Terradiff
  ( Config
  , options
  , someFunc
  , run
  ) where

import Protolude

import qualified Data.Attoparsec.Text as A
import qualified JmlSvc
import qualified Options.Applicative as Opt

import qualified Terradiff.API as API
import Terradiff.Duration (Duration)
import qualified Terradiff.Duration as Duration
import qualified Terradiff.Poll as Poll
import qualified Terradiff.Terraform as Terraform

-- | Overall command-line configuration.
data Config
  = Config
  { serverConfig :: JmlSvc.Config
  , apiConfig :: API.APIConfig
  , terraformConfig :: Terraform.FlagConfig
  , pollInterval :: Duration
  }
  deriving (Eq, Show)

-- | Command-line parser for terradiff.
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
        , Opt.progDesc "Daemonized 'terraform plan'"
        , Opt.header "terradiff - see how Terraform configuration differs from reality"
        ]

-- | Run the terradiff API server.
run :: Config -> IO ()
run Config{serverConfig, apiConfig, terraformConfig, pollInterval} = do
  tfConfig <- Terraform.validateFlagConfig terraformConfig
  initResult <- Terraform.init tfConfig
  case Terraform.processExitCode initResult of
    ExitFailure n ->
      die (toS ("'terraform init' failed: " <> show n <> "\n" <>
                "output:\n" <> Terraform.processOutput initResult <> "\n" <>
                "error:\n" <> Terraform.processError initResult <> "\n"))
    _ ->
      Poll.runWhilePolling (runExceptT $ Terraform.diff tfConfig) (Duration.toDiffTime pollInterval)
        (JmlSvc.run "terradiff" serverConfig . API.app apiConfig (Terraform.terraformPath tfConfig))

someFunc :: Int -> Int -> Int
someFunc x y = x + y

