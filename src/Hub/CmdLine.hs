-- Copyright (C) 2016-2019 Ulf Leopold
--
module Hub.CmdLine
  ( getAppConfig
  , AppConfig(..)
  ) where

import Control.Applicative ((<**>), (<*>))
import Options.Applicative
  ( Parser
  , argument
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , many
  , metavar
  , optional
  , short
  , str
  , strOption
  , switch
  )

data AppConfig = AppConfig
  { profile :: Bool
  , config :: Maybe String
  , dryrun :: Bool
  , tags :: [String]
  } deriving (Show)

cmdOpts :: Parser AppConfig
cmdOpts =
  AppConfig <$> switch (long "profile" <> short 'p' <> help "Enable profiling") <*>
  optional
    (strOption
       (long "config" <> short 'c' <> help "Path to user configuration file")) <*>
  switch
    (long "dry-run" <> short 'd' <>
     help "Print the selected command, don't execute it") <*>
  many (argument str (metavar "TAGS"))

getAppConfig :: IO AppConfig
getAppConfig = execParser opts
  where
    opts =
      info
        (cmdOpts <**> helper)
        (fullDesc <> header "Hub, Copyright (C) 2016-2019 Ulf Leopold")
