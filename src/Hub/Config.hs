-- Copyright (C) 2016 Ulf Leopold
--
module Hub.Config (Command, readConfig, filterCmds, mapCmds, getShellCmd) where

import Hub.CmdLine
import System.Directory
import System.FilePath
import Cheapskate
import qualified Data.Text as DT
import qualified Data.Text.Lazy as TL
import qualified Data.Sequence as SEQ
import Data.List
import Text.Printf (printf)
import qualified Language.Haskell.Interpreter as I
import System.CPUTime
import Data.Either
import qualified Language.Scheme.Core as S

type Tags = [String]
data Command = Command Tags String deriving (Ord, Show, Eq)

readConfig :: AppConfig -> IO [Command]
readConfig appCfg = do
    home <- getHomeDirectory
    let configFiles =
            map (\x -> joinPath [home, x]) [".hub.scm", ".hub.hs", ".hub.md"]
    cfgFiles <- firstExisting configFiles
    case cfgFiles of
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
fileToCmds :: FilePath -> IO [Command]
fileToCmds filepath = do
  case takeExtension filepath of
    ".scm" -> schemeFileToCmds filepath
    ".hs" -> haskellFileToCmds filepath
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
    start <- getCPUTime
    config <- interpretScheme filePath
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
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

markdownFileToCmds :: FilePath -> IO [Command]
markdownFileToCmds filePath = do
    blocks <- fileToBlocks filePath
    cmds <- parseBlocks blocks [(0, [])] []
    return (reverse cmds)

fileToBlocks :: FilePath -> IO Blocks
fileToBlocks fileName = do
    contents <- readFile fileName
    let Doc options blocks = toMarkdown (DT.pack contents)
    return blocks

toMarkdown :: DT.Text -> Doc
toMarkdown = markdown def

parseBlocks :: Blocks -> [(Int, [String])] -> [Command] -> IO [Command]
parseBlocks blocks tags result = do
    case null blocks of
        True -> return result
        False -> do
            let (currentLevel, _) = head tags
            (tags1, result1, nb) <-
                case SEQ.index blocks 0 of
                    Header headerLevel inlines ->
                        case (compare headerLevel currentLevel) of
                            GT ->
                                return
                                    ( (headerLevel, (inlinesToWords inlines)) :
                                      tags
                                    , result
                                    , SEQ.empty)
                            EQ ->
                                return
                                    ( (headerLevel, (inlinesToWords inlines)) :
                                      (tail tags)
                                    , result
                                    , SEQ.empty)
                            LT ->
                                return
                                    ( (headerLevel, (inlinesToWords inlines)) :
                                      (dropPastLevel tags headerLevel)
                                    , result
                                    , SEQ.empty)
                    CodeBlock codeattr text ->
                        case codeattr of
                            CodeAttr {codeLang = lang
                                     ,codeInfo = info}
                                | lang == (DT.pack "include") -> do
                                    b <- fileToBlocks (DT.unpack text)
                                    return (tags, result, b)
                                | otherwise ->
                                    return
                                        ( tags
                                        , (Command
                                               (reverse (tagsFromLevelList tags))
                                               (DT.unpack text)) :
                                          result
                                        , SEQ.empty)
                    _ -> return (tags, result, SEQ.empty)
            parseBlocks ((SEQ.><) nb (SEQ.drop 1 blocks)) tags1 result1

dropPastLevel [] headerLevel = []
dropPastLevel ((lev, tags):rest) headerLevel 
    | lev <= headerLevel = rest
    | otherwise = dropPastLevel rest headerLevel

tagsFromLevelList [] = []
tagsFromLevelList ((lev, tags):rest) =
    tags ++ tagsFromLevelList rest

inlinesToWords inlines = inlinesToWordsHelp inlines []

inlinesToWordsHelp :: Inlines -> [String] -> [String]
inlinesToWordsHelp inlines acc =
    case null inlines of
        True -> acc
        False ->
            case SEQ.index inlines 0 of
                Str text ->
                    inlinesToWordsHelp (SEQ.drop 1 inlines) ((DT.unpack text) : acc)
                Space -> inlinesToWordsHelp (SEQ.drop 1 inlines) acc
                _ -> error "Only words and spaces allowed in headings"
