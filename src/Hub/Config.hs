-- Copyright (C) 2016 Ulf Leopold
--
module Hub.Config
  ( readConfig
  , filterCmds
  , mapCmds
  , getShellCmd
  ) where
import Hub.CmdLine
import System.Directory
import System.FilePath
import Data.List
import Text.Printf (printf)
import System.CPUTime
import Data.Either

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

filterCmds :: [String] -> [Command] -> [Command]
filterCmds keywords cmds =
    let filtered =
            foldl
                (\acc keyword ->
                      filter (\(Command kws _) -> keyword `elem` kws) acc)
                cmds
                keywords
    in map (\(Command kws cmd) -> Command (kws \\ keywords) cmd) filtered

mapCmds :: [Command] -> (String -> String -> b) -> [b]
mapCmds commands fun =
    map (\(Command tags cmd) -> fun (unwords tags) cmd) commands

getShellCmd :: Command -> String
getShellCmd (Command tags shellCmd) = shellCmd

-- Internal ============================================================
configFile appCfg = do fileCandidates appCfg >>= firstExisting

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
loadCommands filepath = do
    case takeExtension filepath of
        ".lua" -> luaFileToCmds filepath
        ".md" -> markdownFileToCmds filepath
        _ -> error "Not supported config file extension."

dt (AppConfig {profile = True}) msg fun = do
    t1 <- getCPUTime
    res <- fun ()
    t2 <- getCPUTime
    let secs = ((fromIntegral (t2 - t1)) / (10^12)) :: Double
    putStrLn (printf msg secs)
    return res
dt (AppConfig {profile = False}) msg fun = 
    fun ()
