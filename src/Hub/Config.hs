-- Copyright (C) 2016 Ulf Leopold
--
module Hub.Config
  ( readConfig
  ) where

import Hub.CmdLine
import System.Directory
import System.FilePath
import Text.Printf (printf)
import System.CPUTime

import Hub.CommandType
import Hub.ConfigMarkdown
import Hub.ConfigLua

readConfig :: AppConfig -> IO [Command]
readConfig appCfg = do
    cfgFile <- configFile appCfg
    case cfgFile of
        Just file ->
            dt
                appCfg
                "Time spent loading configs: %0.3f sec\n"
                (\_ -> loadCommands file)
        Nothing -> return []

configFile :: AppConfig -> IO (Maybe FilePath)
configFile appCfg = fileCandidates appCfg >>= firstExisting

fileCandidates :: AppConfig -> IO [FilePath]
fileCandidates appCfg = do
    home <- getHomeDirectory
    let defaultFiles =
            map (\x -> joinPath [home, x]) [".hub.lua", ".hub.md"]
    case config appCfg of
      "" -> return defaultFiles
      x -> return [x]

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting [] = return Nothing
firstExisting (filePath : filePaths) = do
  res <- doesFileExist filePath
  if res then
      return (Just filePath)
  else
      firstExisting filePaths

loadCommands :: FilePath -> IO [Command]
loadCommands filepath =
    case takeExtension filepath of
        ".lua" -> luaFileToCmds filepath
        ".md" -> markdownFileToCmds filepath
        _ -> error "Not supported config file extension."

dt :: AppConfig -> String -> (() -> IO b) -> IO b
dt AppConfig {profile = True} msg fun = do
    t1 <- getCPUTime
    res <- fun ()
    t2 <- getCPUTime
    let secs = fromIntegral (t2 - t1) / ((10^(12 :: Int)) :: Double)
    putStrLn (printf msg secs)
    return res
dt AppConfig {profile = False} _ fun =
    fun ()
