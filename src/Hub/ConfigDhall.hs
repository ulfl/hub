-- Copyright (C) Ulf Leopold
--
module Hub.ConfigDhall
  ( dhallFileToCmds
  ) where

import Data.String.Here (here)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Dhall (auto, detailed, input)
import Hub.CommandType (Command)

dhallFileToCmds :: FilePath -> IO [Command]
dhallFileToCmds filePath = do
  config <- Data.Text.IO.readFile filePath
  detailed (input auto config)
