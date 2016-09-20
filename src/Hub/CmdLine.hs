-- Copyright (C) 2016 Ulf Leopold
--
{-# LANGUAGE DeriveDataTypeable #-}
module Hub.CmdLine
  ( getAppConfig
  , AppConfig(AppConfig, profile, config, tags)
  ) where

import System.Console.CmdArgs

data AppConfig = AppConfig
    { profile :: Bool
    , config :: String
    , tags :: [String]
    } deriving (Show, Data, Typeable)

cfg =
    AppConfig
    { profile = def &= name "p" &= help "Enable profiling"
    , config = def &= name "c" &= help "Path to user configuration file"
    , tags = def &= args
    } &=
    summary "Hub, Copyright (C) 2016 Ulf Leopold"

getAppConfig :: IO AppConfig
getAppConfig = cmdArgs cfg
