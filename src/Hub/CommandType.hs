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

makeCmd :: Tags -> String -> Command
makeCmd = Command

getShellCmd :: Command -> String
getShellCmd (Command _ shellCmd) = shellCmd

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
                          (not . isMatch (dropTagPrefix tag))
                          cmds)
                     tag)
        (False, True) ->
            filterCmdsAndTags
                tags
                (filterCmds
                     (isPartialMatch (dropTagPrefix tag))
                     cmds)
        _ ->
            filterCmdsAndTags
                tags
                (removeTag
                     (filterCmds (isMatch tag) cmds)
                     tag)

isExcludeTag :: String -> Bool
isExcludeTag = isPrefixOf "!"

isPartialMatchTag :: String -> Bool
isPartialMatchTag = isPrefixOf "/"

filterCmds :: ([String] -> Bool) -> [Command] -> [Command]
filterCmds fun =
    filter (\(Command tags _) -> fun tags)

dropTagPrefix :: String -> String
dropTagPrefix = drop 1

isMatch :: String -> Tags -> Bool
isMatch tag tags = tag `elem` tags

isPartialMatch :: String -> Tags -> Bool
isPartialMatch partialTag = any (isInfixOf partialTag)

removeTag :: [Command] -> String -> [Command]
removeTag cmds tag =
    map (\(Command tags cmd) -> Command (tags \\ [tag]) cmd) cmds
