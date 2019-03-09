-- Copyright (C) 2016-2019 Ulf Leopold
--
module Hub.ConfigLua (luaFileToCmds) where

import Hub.CommandType
import Hub.ConfigLuaPrelude

import Foreign.Lua

luaFileToCmds :: FilePath -> IO [Command]
luaFileToCmds filePath = runLua $ do
    openlibs
    _ <- loadstring hubLuaPrelude
    call 0 0
    _ <-loadfile filePath
    call 0 1
    peek (-1)
