-- | Terraforming with style.
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
