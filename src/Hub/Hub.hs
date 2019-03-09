-- Copyright (C) 2016-2019 Ulf Leopold
--
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE OverloadedStrings #-}

module Hub.Hub (hub) where

import Hub.CmdLine

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import qualified Data.Char
import Brick.Types (Widget)
import Brick.Widgets.Core
       (str, vLimit, hLimit, hBox, vBox, withAttr, padLeft, padRight,
        fill)
import Brick.Util (on)
import qualified Data.Text.Zipper as Z
import qualified Text.Printf

import System.Process

import Hub.CommandType
import qualified Hub.Config as Hc

import Control.Lens ((^.))

data FieldName
    = ListField
    | EditField
     deriving (Ord, Show, Eq)

data State =
    State (L.List FieldName ListRow)  -- The list widget.
          (E.Editor String FieldName) -- The editor widget.
          [Command]                   -- List of available 'Commands'.
          Action                      -- Action to take on exit.

data ListRow = ListRow String String deriving (Ord, Show, Eq)

type Cmd = String
data Action = Run Cmd | Print Cmd | JustExit

hub :: IO ()
hub = do
    appCfg <- getAppConfig
    cmds <- Hc.readConfig appCfg
    let filteredCmds = filterCmdsAndTags (tags appCfg) cmds
    cmd <-
        case filteredCmds of
            [cmd] -> return (Run (getShellCmd cmd))
            _ -> do
                State _ _ _ cmd <-
                    M.defaultMain theApp (initialState filteredCmds)
                return cmd
    action appCfg cmd

action :: AppConfig -> Action -> IO ()
action appCfg (Run cmd) =
    if not (dryrun appCfg) then
        callCommand cmd
    else
        putStrLn cmd
action _ (Print cmd) = putStrLn cmd
action _ JustExit = return ()

-- Internal ============================================================
initialState :: [Command] -> State
initialState cmds =
    State
        (L.list ListField (Vec.fromList (commandsToRows cmds)) 2)
        (E.editor EditField (Just 1) "")
        cmds
        JustExit

theApp :: M.App State V.Event FieldName
theApp =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theAttrMap
    }

drawUI :: State -> [Widget FieldName]
drawUI state = [ui]
  where
    State l e cmds _ = state
    box = L.renderList listDrawElement False l
    prompt = E.renderEditor (str . unlines) True e
    ui =
        vBox
            [ box
            , hBox
                  [ withAttr
                        L.listSelectedAttr
                        (str
                             (Text.Printf.printf
                                  "Showing %d of %d items."
                                  (Vec.length (l^.L.listElementsL))
                                  (length cmds)))
                  , withAttr L.listSelectedAttr (vLimit 1 (fill ' '))]
            , hBox [str "hub> ", prompt]]

listDrawElement :: Bool -> ListRow -> Widget FieldName
listDrawElement sel (ListRow tags description) =
    let attr =
            if sel
                then customAttr
                else L.listAttr
    in hBox
           [ withAttr attr (vLimit 2 (hLimit 1 (fill ' ')))
           , vLimit 2 (hLimit 1 (fill ' '))
           , padRight
                 T.Max
                 (vBox [str tags, padLeft (T.Pad 8) (str description)])]

appEvent :: State -> T.BrickEvent FieldName e -> T.EventM FieldName (T.Next State)
appEvent (State l ed commands action) be = case be of
    T.VtyEvent e -> case e of
        V.EvKey (V.KChar 'n') [V.MCtrl] ->
            M.continue (State (L.listMoveDown l) ed commands action)
        V.EvKey V.KDown [] ->
            M.continue (State (L.listMoveDown l) ed commands action)
        V.EvKey (V.KChar 'p') [V.MCtrl] ->
            M.continue (State (L.listMoveUp l) ed commands action)
        V.EvKey V.KUp [] -> M.continue (State (L.listMoveUp l) ed commands action)
        V.EvKey (V.KChar 'w') [V.MCtrl] ->
            let words = head (E.getEditContents ed)
                len = lengthOfPrevWord words
                ed2 =
                    foldl (\a _ -> E.applyEdit Z.deletePrevChar a) ed [1 .. len]
                l2 = updateDisplayList l ed2 commands
            in M.continue (State l2 ed2 commands action)
        V.EvKey V.KEnter [] ->
            let action =
                    case L.listSelectedElement l of
                        Just (_, ListRow _ cmd) -> Run cmd
                        Nothing -> JustExit
            in M.halt (State l ed commands action)
        V.EvKey V.KEsc [] ->
            let action =
                    case L.listSelectedElement l of
                        Just (_, ListRow _ cmd) -> Print cmd
                        Nothing -> JustExit
            in M.halt (State l ed commands action)
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt (State l ed commands JustExit)
        _ -> do
            ed2 <- E.handleEditorEvent e ed
            let l2 = updateDisplayList l ed2 commands
            M.continue (State l2 ed2 commands action)
    _ -> M.continue (State l ed commands action)

lengthOfPrevWord :: String -> Int
lengthOfPrevWord str =
    let str1 =
            dropWhile
                (not . Data.Char.isSpace)
                (dropWhile Data.Char.isSpace (reverse str))
    in length str - length str1

updateDisplayList
    :: L.List FieldName ListRow
    -> E.Editor String FieldName
    -> [Command]
    -> L.List FieldName ListRow
updateDisplayList l ed commands =
    L.listReplace
        (Vec.fromList
             (let words = getUserInputWords (head (E.getEditContents ed))
              in commandsToRows (filterCmdsAndTags words commands)))
        (Just 0)
        l

getUserInputWords :: String -> [String]
getUserInputWords [] = []
getUserInputWords s =
    let wordList = words s
        completed = last s == ' ' || isLastTagPartialMatch wordList
    in if completed
           then wordList
           else init wordList

isLastTagPartialMatch :: [String] -> Bool
isLastTagPartialMatch [] = False
isLastTagPartialMatch tags = isPartialMatchTag (last tags)

commandsToRows :: [Command] -> [ListRow]
commandsToRows commands =
    mapCmds commands ListRow

theAttrMap :: A.AttrMap
theAttrMap =
    A.attrMap
        V.defAttr
        [ (L.listAttr, V.defAttr)
        , (L.listSelectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (customAttr, V.white `on` V.red `V.withStyle` V.defaultStyleMask)]

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"
