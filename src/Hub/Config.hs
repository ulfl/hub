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

import qualified Language.Haskell.Interpreter as I
import qualified Language.Scheme.Core as S

import Hub.CommandType
import Hub.ConfigMarkdown
import Hub.ConfigLua

readConfig :: AppConfig -> IO [Command]
readConfig appCfg = do
    cfgFiles <- getConfigFiles appCfg
    cfgFile <- firstExisting cfgFiles
    case cfgFile of
        Just file ->
            dt
                appCfg
                "Time spent loading configs: %0.3f sec\n"
                (\_ -> fileToCmds file)
        Nothing -> return []

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting [] = return Nothing
firstExisting (filePath : filePaths) = do
  res <- doesFileExist filePath
  if res then
      return (Just filePath)
  else
      firstExisting filePaths

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
getConfigFiles appCfg = do
    home <- getHomeDirectory
    let defaultFiles =
            map (\x -> joinPath [home, x]) [".hub.scm", ".hub.hs", ".hub.md"]
    case userConfig appCfg of
      "" -> return defaultFiles
      x -> return [x]

fileToCmds :: FilePath -> IO [Command]
fileToCmds filepath = do
    case takeExtension filepath of
        ".md" -> markdownFileToCmds filepath
        ".lua" -> luaFileToCmds filepath
        ".scm" -> schemeFileToCmds filepath
        ".hs" -> haskellFileToCmds filepath
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

haskellFileToCmds :: FilePath -> IO [Command]
haskellFileToCmds filePath = do
    config <- interpretHaskell filePath
    toCommandList config

interpretHaskell :: FilePath -> IO [(String, [String], String)]
interpretHaskell filePath = do
    res <-
        I.runInterpreter $
        do I.loadModules [filePath]
           I.setImports ["Prelude", "HubConfig"]
           I.interpret
                "HubConfig.main"
                (I.as :: [(String, [String], String)])
    case res of
        Left err ->
            error (printf "Couldn't evaluate Hub config (%s)." (show err))
        Right x -> return x

schemeFileToCmds :: FilePath -> IO [Command]
schemeFileToCmds filePath = do
    config <- interpretScheme filePath
    toCommandList config

interpretScheme :: FilePath -> IO [(String, [String], String)]
interpretScheme filePath = do
  contents <- readFile filePath
  env <- S.r5rsEnv
  res <- S.evalString env contents
  return [("Command", [], "ssh")]

toCommandList :: [(String, [String], String)] -> IO [Command]
toCommandList [] = return []
toCommandList (("Command", tags, cmd) : rest)  = do
  res <- toCommandList rest
  return ((Command tags cmd) : res)
toCommandList (("Include", tags, includePath) : rest) = do
  res <- fileToCmds includePath
  let resWithTags = addTags tags res
  cmds2 <- toCommandList rest
  return (resWithTags ++ cmds2)

addTags tags cmds =
    map (\(Command t cmd) -> (Command (tags ++ t) cmd)) cmds
