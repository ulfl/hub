-- Copyright (C) Ulf Leopold
--
module Hub.CmdLine
  ( getAppConfig
  , AppConfig(..)
  ) where

import Control.Applicative ((<**>))
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

data AppConfig =
  AppConfig
    { profile :: Bool
    , config :: Maybe String
    , dryrun :: Bool
    , stdOut :: Bool
    , tags :: [String]
    }
  deriving (Show)

cmdOpts :: Parser AppConfig
cmdOpts =
  AppConfig <$> switch (long "profile" <> short 'p' <> help "Enable profiling") <*>
  optional
    (strOption
       (long "config" <> short 'c' <> help "Path to user configuration file")) <*>
  switch
    (long "dry-run" <>
     short 'd' <> help "Print the selected command, don't execute it") <*>
  switch
    (long "stdout" <>
     short 'o' <> help "Print commands to stdout. No curses GUI.") <*>
  many (argument str (metavar "TAGS"))

getAppConfig :: IO AppConfig
getAppConfig = execParser opts
  where
    opts =
      info
        (cmdOpts <**> helper)
        (fullDesc <> header "Hub, Copyright (C) Ulf Leopold")
