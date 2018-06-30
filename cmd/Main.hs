-- | Run the mass-driver API server
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
