-- Copyright (C) 2016 Ulf Leopold
--
module Hub.CommandType
  ( Command
  , makeCmd
  , getShellCmd
  , mapCmds
  , filterCmdsAndTags
  , isPartialMatchTag
  ) where

import Data.List

type Tags = [String]
data Command = Command Tags String deriving (Ord, Show, Eq)

makeCmd tags cmd = Command tags cmd

getShellCmd :: Command -> String
getShellCmd (Command tags shellCmd) = shellCmd

mapCmds :: [Command] -> (String -> String -> b) -> [b]
mapCmds commands fun =
    map (\(Command tags cmd) -> fun (unwords tags) cmd) commands

filterCmdsAndTags :: [String] -> [Command] -> [Command]
filterCmdsAndTags [] cmds = cmds
filterCmdsAndTags (tag:tags) cmds =
    case (isExcludeTag tag, isPartialMatchTag tag) of
        (True, False) ->
            filterCmdsAndTags
                tags
                (removeTag
                     (filterCmds
                          (\cmdTags -> not (isMatch (dropTagPrefix tag) cmdTags))
                          cmds)
                     tag)
        (False, True) ->
            filterCmdsAndTags
                tags
                (filterCmds
                     (\cmdTags -> isPartialMatch (dropTagPrefix tag) cmdTags)
                     cmds)
        _ ->
            filterCmdsAndTags
                tags
                (removeTag
                     (filterCmds (\cmdTags -> isMatch tag cmdTags) cmds)
                     tag)

isExcludeTag x = isPrefixOf "!" x
isPartialMatchTag x = isPrefixOf "/" x

filterCmds :: ([String] -> Bool) -> [Command] -> [Command]
filterCmds fun commands =
    filter (\(Command tags cmd) -> fun tags) commands

dropTagPrefix tag = (drop 1) tag

isMatch tag tags = tag `elem` tags

isPartialMatch partialTag tags = any (isInfixOf partialTag) tags

removeTag :: [Command] -> String -> [Command]
removeTag cmds tag =
    map (\(Command cmdTags cmd) -> Command (cmdTags \\ [tag]) cmd) cmds
