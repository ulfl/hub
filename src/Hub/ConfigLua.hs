-- Copyright (C) 2016 Ulf Leopold
--
module Hub.ConfigLua (luaFileToCmds) where

import Hub.CommandType

import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Scripting.Lua

luaFileToCmds :: FilePath -> IO [Command]
luaFileToCmds filePath = do
    l <- newstate
    openlibs l
    registerhsfunction l "helloWorld" helloWorld
    loadfile l filePath
    call l 0 1
    x <- tointeger l 0
    putStrLn $ show x
    close l
    return []

helloWorld :: IO B.ByteString
helloWorld = return $ B.pack "Hello, World!"
