-- Copyright (C) 2016 Ulf Leopold
--
{-# OPTIONS_GHC -fno-ignore-asserts #-}

module Hub.ConfigLua (luaFileToCmds) where

import Hub.CommandType
import Hub.ConfigLuaPrelude

import Scripting.Lua as Lua
import qualified Data.ByteString.Char8 as B
import Control.Exception (assert)

luaFileToCmds :: FilePath -> IO [Command]
luaFileToCmds filePath = do
    l <- newstate
    openlibs l
    loadstring l hubLuaPrelude "hubPrelude"
    call l 0 0
    loadfile l filePath
    call l 0 1
    cmds <- getCommands l
    close l
    return cmds

getCommands :: LuaState -> IO [Command]
getCommands l = do
    istable l (-1) >>= do (flip assert) (return ())
    len <- objlen l 0
    getCommandsHelper l [] len

getCommandsHelper :: LuaState -> [Command] -> Int -> IO [Command]
getCommandsHelper l acc 0 = do return acc 
getCommandsHelper l acc count = do
    rawgeti l (-1) count
    ls <- getCommand l
    pop l 1
    getCommandsHelper l (ls:acc) (count - 1)

getCommand :: LuaState -> IO Command
getCommand l = do
    rawgeti l (-1) 1
    tags <- getListOfStrings l
    pop l 1
    rawgeti l (-1) 2
    cmd <- tostring l (-1)
    pop l 1
    return (makeCmd tags (B.unpack cmd))

getListOfStrings l = do
    istable l (-1) >>= do (flip assert) (return ())
    len <- objlen l (-1)
    getListOfStringsHelper l [] len

getListOfStringsHelper l acc 0 = do return acc
getListOfStringsHelper l acc count = do
    istable l (-1) >>= do (flip assert) (return ())
    rawgeti l (-1) count
    s <- tostring l (-1)
    pop l 1
    getListOfStringsHelper l ((B.unpack s):acc) (count - 1)
