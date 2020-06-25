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
  commands <- detailed (input auto (prelude <> config))
  return commands

prelude :: Text
prelude =
  [here|
let map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/0a7f596d03b3ea760a96a8e03935f4baa64274e1/Prelude/List/map sha256:310614c2949366621d7848e37c7845090d6c5b644234a1defe4d4048cf124fcd

let Command : Type = { tags : List Text, shellCommand : Text }

let cmd = λ(t : List Text) → λ(c : Text) → { tags = t, shellCommand = c }

let tags =
        λ(t : List Text)
      → λ(cmds : List Command)
      → map
        Command
        Command
        (λ(c : Command) → { tags = t # c.tags, shellCommand = c.shellCommand })
        cmds

let cmds =
        λ(tags : List Text)
      → λ(vars : List Text)
      → λ(fmt : Text → Text)
      → map Text Command (λ(var : Text) → cmd tags (fmt var)) vars

let web =
        λ(t : List Text)
      → λ(c : Text)
      → { tags = t, shellCommand = (env:OPENCMD as Text ? "open") ++ " " ++ c }
|]
