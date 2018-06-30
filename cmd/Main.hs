module Main (main) where

import Protolude

import qualified Options.Applicative as Opt

import qualified Lib

main :: IO ()
main = do
  config <- Opt.execParser Lib.options
  let result = Lib.someFunc 2 3
  putText $ "Config = " <> show config
  putText $ "Result = " <> show result
