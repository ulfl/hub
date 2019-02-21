-- Copyright (C) 2016 Ulf Leopold
--
module Hub.ConfigMarkdown (markdownFileToCmds) where

import Hub.CommandType
import Cheapskate
import qualified Data.Text as DT
import qualified Data.Sequence as SEQ

markdownFileToCmds :: FilePath -> IO [Command]
markdownFileToCmds filePath = do
    blocks <- fileToBlocks filePath
    cmds <- parseBlocks blocks [(0, [])] []
    return (reverse cmds)

fileToBlocks :: FilePath -> IO Blocks
fileToBlocks fileName = do
    contents <- readFile fileName
    let Doc _ blocks = toMarkdown (DT.pack contents)
    return blocks

toMarkdown :: DT.Text -> Doc
toMarkdown = markdown def

parseBlocks :: Blocks -> [(Int, [String])] -> [Command] -> IO [Command]
parseBlocks blocks _ _ | null blocks = return []
parseBlocks blocks tags result = do
    let (currentLevel, _) = head tags
    (tags1, result1, nb) <-
        case SEQ.index blocks 0 of
            Header headerLevel inlines ->
                case compare headerLevel currentLevel of
                    GT ->
                        return
                            ( (headerLevel, inlinesToWords inlines) :
                              tags
                            , result
                            , SEQ.empty)
                    EQ ->
                        return
                            ( (headerLevel, inlinesToWords inlines) :
                              tail tags
                            , result
                            , SEQ.empty)
                    LT ->
                        return
                            ( (headerLevel, inlinesToWords inlines) :
                              dropPastLevel tags headerLevel
                            , result
                            , SEQ.empty)
            CodeBlock codeattr text ->
                case codeattr of
                    CodeAttr {codeLang = lang}
                        | lang == DT.pack "include" -> do
                            b <- fileToBlocks (DT.unpack text)
                            return (tags, result, b)
                        | otherwise ->
                            return
                                ( tags
                                , makeCmd
                                      (reverse (tagsFromLevelList tags))
                                      (DT.unpack text) :
                                  result
                                , SEQ.empty)
            _ -> return (tags, result, SEQ.empty)
    parseBlocks ((SEQ.><) nb (SEQ.drop 1 blocks)) tags1 result1

dropPastLevel :: Ord a => [(a, b)] -> a -> [(a, b)]
dropPastLevel [] _ = []
dropPastLevel ((lev, _):rest) headerLevel
    | lev <= headerLevel = rest
    | otherwise = dropPastLevel rest headerLevel

tagsFromLevelList :: [(a, [b])] -> [b]
tagsFromLevelList [] = []
tagsFromLevelList ((_, tags):rest) =
    tags ++ tagsFromLevelList rest

inlinesToWords :: Inlines -> [String]
inlinesToWords inlines = inlinesToWordsHelp inlines []

inlinesToWordsHelp :: Inlines -> [String] -> [String]
inlinesToWordsHelp inlines acc | null inlines = acc
inlinesToWordsHelp inlines acc =
    case SEQ.index inlines 0 of
        Str text ->
            inlinesToWordsHelp (SEQ.drop 1 inlines) (DT.unpack text : acc)
        Space -> inlinesToWordsHelp (SEQ.drop 1 inlines) acc
        _ -> error "Only words and spaces allowed in headings"
