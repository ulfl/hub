-- Copyright (C) 2016 Ulf Leopold
--
{-# OPTIONS_GHC -fno-ignore-asserts #-}

module Hub.ConfigLua (luaFileToCmds) where

import Hub.CommandType
import Hub.ConfigLuaPrelude

import Foreign.Lua
import qualified Data.ByteString.Char8 as B
import Control.Exception (assert)

luaFileToCmds :: FilePath -> IO [Command]
luaFileToCmds filePath = runLua $ do
    openlibs
    loadstring hubLuaPrelude
    call 0 0
    loadfile filePath
    call 0 1
    getCommands

getCommands :: Lua [Command]
getCommands = do
    istable (-1) >>= flip assert (return ())
    len <- rawlen (-1)
    getCommandsHelper [] len

getCommandsHelper :: [Command] -> Int -> Lua [Command]
getCommandsHelper acc 0 = return acc
getCommandsHelper acc count = do
    rawgeti (-1) count
    ls <- getCommand
    pop 1
    getCommandsHelper (ls:acc) (count - 1)

getCommand :: Lua Command
getCommand = do
    rawgeti (-1) 1
    tags <- getListOfStrings
    pop 1
    rawgeti (-1) 2
    cmd <- tostring (-1)
    pop 1
    return (makeCmd tags (B.unpack cmd))

getListOfStrings = do
    istable (-1) >>= flip assert (return ())
    len <- rawlen (-1)
    getListOfStringsHelper [] len

getListOfStringsHelper acc 0 = return acc
getListOfStringsHelper acc count = do
    istable (-1) >>= flip assert (return ())
    rawgeti (-1) count
    s <- tostring (-1)
    pop 1
    getListOfStringsHelper (B.unpack s : acc) (count - 1)
