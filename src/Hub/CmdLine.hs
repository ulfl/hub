-- Copyright (C) 2016 Ulf Leopold
--
{-# LANGUAGE DeriveDataTypeable #-}
module Hub.CmdLine
  ( getAppConfig
  , AppConfig(AppConfig, profile, config, dryrun, tags)
  ) where

import System.Console.CmdArgs

data AppConfig = AppConfig
    { profile :: Bool
    , config :: String
    , dryrun :: Bool
    , tags :: [String]
    } deriving (Show, Data, Typeable)

cfg :: AppConfig
cfg =
    AppConfig
    { profile = def &= name "p" &= help "Enable profiling"
    , config = def &= name "c" &= help "Path to user configuration file"
    , dryrun =
        def &= name "d" &= help "Print the selected command, don't execute it"
    , tags = def &= args
    } &=
    summary "Hub, Copyright (C) 2016 Ulf Leopold"

getAppConfig :: IO AppConfig
getAppConfig = cmdArgs cfg
