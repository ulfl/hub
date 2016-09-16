-- Copyright (C) 2016 Ulf Leopold
--
module Hub.Config (Command, readConfig, filterCmds, mapCmds, getShellCmd) where

import System.Directory
import System.FilePath
import Cheapskate
import qualified Data.Text as DT
import qualified Data.Text.Lazy as TL
import qualified Data.Sequence as SEQ
import Data.List
import qualified Language.Haskell.Interpreter as I
import Text.Printf (printf)

type Tags = [String]
data Command = Command Tags String deriving (Ord, Show, Eq)

readConfig = do
    home <- getHomeDirectory
    let haskellConfigFile = joinPath [home, ".hub.hs"]
    let markdownConfigFile = joinPath [home, ".hub.md"]
    exists <- doesFileExist haskellConfigFile
    let configFile =
            if exists
                then haskellConfigFile
                else markdownConfigFile
    fileToCmds configFile

filterCmds :: [String] -> [Command] -> [Command]
filterCmds [] cmds = cmds
filterCmds (k:keywords) cmds =
    filter
        (\(Command keywords _) -> k `elem` keywords)
        (filterCmds keywords cmds)

mapCmds :: [Command] -> [String] -> (String -> String -> b) -> [b]
mapCmds commands words fun =
    map (\(Command tags cmd) -> fun (unwords (tags \\ words)) cmd) commands

getShellCmd :: Command -> String
getShellCmd (Command tags shellCmd) = shellCmd

-- Internal ============================================================
fileToCmds :: FilePath -> IO [Command]
fileToCmds filepath =
    case takeExtension filepath of
        ".hs" -> haskellFileToCmds filepath
        ".md" -> markdownFileToCmds filepath
        _ -> error "Not supported config file extension."

haskellFileToCmds :: FilePath -> IO [Command]
haskellFileToCmds filePath = do
    config <- interpret filePath
    toCommandList config

interpret :: FilePath -> IO [(String, [String], String)]
interpret filePath = do
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

toCommandList :: [(String, [String], String)] -> IO [Command]
toCommandList [] = return []
toCommandList (("Command", tags, cmd) : rest)  = do
  res <- toCommandList rest
  return ((Command tags cmd) : res)
toCommandList (("Include", tags, includePath) : rest) = do
  res <- interpret includePath
  let resWithTags = addTags tags res
  cmds1 <- toCommandList resWithTags
  cmds2 <- toCommandList rest
  return (cmds1 ++ cmds2)

addTags tags cmds = map (\(typeStr, t, cmd) -> (typeStr, tags ++ t, cmd)) cmds

markdownFileToCmds :: String -> IO [Command]
markdownFileToCmds fileName = do
    blocks <- fileToBlocks fileName
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
