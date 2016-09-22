-- Copyright (C) 2016 Ulf Leopold
--
module Hub.CommandType
  ( Command
  , makeCmd
  , getShellCmd
  , mapCmds
  , filterCmds
  , filterCmdsAndTags
  , removeTags
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

filterCmds :: ([String] -> Bool) -> [Command] -> [Command]
filterCmds fun commands =
    filter (\(Command tags cmd) -> fun tags) commands

filterCmdsAndTags :: [String] -> [Command] -> [Command]
filterCmdsAndTags searchTags cmds =
    let (tagsToExclude, tagsToInclude) = partition (\x -> isPrefixOf "!" x) searchTags
        tagsToExclude1 = map (drop 1) tagsToExclude
        included = doFilter tagsToInclude cmds True
        excludedRemoved = doFilter tagsToExclude1 included False
    in removeTags excludedRemoved tagsToInclude

doFilter searchTags cmds inclusion =
    let op =
            if inclusion
                then id
                else not
    in foldl (\acc tag -> filterCmds (\tags -> op (tag `elem` tags)) acc) cmds searchTags

removeTags :: [Command] -> [String] -> [Command]
removeTags cmds tagsToRemove =
    map (\(Command tags cmd) -> Command (tags \\ tagsToRemove) cmd) cmds
