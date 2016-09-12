-- Copyright (C) 2016 Ulf Leopold
--
{-# LANGUAGE OverloadedStrings #-}
module Hub.Hub (hub) where

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import qualified Data.Char
import Brick.Types (Widget)
import Brick.Widgets.Core
       (str, vLimit, hLimit, hBox, vBox, withAttr, padLeft, padRight,
        fill)
import Brick.Util (fg, on)
import Data.Monoid
import qualified Data.Text.Zipper as Z
import qualified Text.Printf

import System.Process
import qualified Hub.Config as Hc

import Control.Lens ((^.))

data FieldName
    = ListField
    | EditField
     deriving (Ord, Show, Eq)

data State =
    State (L.List FieldName ListRow) -- The list widget.
          (E.Editor FieldName)       -- The editor widget.
          [Hc.Command]               -- List of available 'Commands'.
          (Maybe String)             -- Command to execute.

data ListRow = ListRow String String deriving (Ord, Show, Eq)

hub :: IO ()
hub = do
    cmds <- Hc.readConfig
    State _ _ _ cmd <- M.defaultMain theApp (initialState cmds)
    case cmd of
        Just cmdStr -> callCommand cmdStr
        Nothing -> return ()
    return ()

-- Internal ============================================================
initialState :: [Hc.Command] -> State
initialState cmds =
    State
        (L.list ListField (Vec.fromList (commandsToRows cmds [])) 2)
        (E.editor EditField (str . unlines) (Just 1) "")
        cmds
        Nothing

theApp :: M.App State V.Event FieldName
theApp =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theAttrMap
    , M.appLiftVtyEvent = id
    }

drawUI :: State -> [Widget FieldName]
drawUI state = [ui]
  where
    State l e cmds _ = state
    box = L.renderList (listDrawElement) False l
    prompt = E.renderEditor True e
    ui =
        vBox
            [ box
            , hBox
                  [ withAttr
                        L.listSelectedAttr
                        (str
                             (Text.Printf.printf
                                  "Showing %d of %d items."
                                  (Vec.length (l^.(L.listElementsL)))
                                  (length cmds)))
                  , withAttr L.listSelectedAttr (vLimit 1 (fill ' '))]
            , hBox [(str "hub> "), prompt]]

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
                 (vBox [str tags, (padLeft (T.Pad 8) (str (description)))])]

appEvent :: State -> V.Event -> T.EventM FieldName (T.Next State)
appEvent (State l ed commands cmd) e =
    case e of
        V.EvKey (V.KChar 'n') [V.MCtrl] ->
            M.continue (State (L.listMoveDown l) ed commands cmd)
        V.EvKey (V.KChar 'p') [V.MCtrl] ->
            M.continue (State (L.listMoveUp l) ed commands cmd)
        V.EvKey (V.KChar 'w') [V.MCtrl] ->
            let words = head (E.getEditContents ed)
                len = lengthOfPrevWord words
                ed2 = foldl (\a x -> E.applyEdit Z.deletePrevChar a) ed [1..len]
                l2 = updateDisplayList l ed2 commands
            in M.continue (State l2 ed2 commands cmd)
        V.EvKey V.KEnter [] ->
            let cmd =
                    case L.listSelectedElement l of
                        Just (_, ListRow tags cmd) -> cmd
                        Nothing -> ""
            in M.halt (State l ed commands (Just cmd))
        V.EvKey V.KEsc [] -> M.halt (State l ed commands Nothing)
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt (State l ed commands Nothing)
        ev -> do
            ed2 <- E.handleEditorEvent e ed
            let l2 = updateDisplayList l ed2 commands
            M.continue (State l2 ed2 commands cmd)

lengthOfPrevWord str =
    let str1 =
            (dropWhile
                 (not . Data.Char.isSpace)
                 ((dropWhile Data.Char.isSpace) (reverse str)))
    in (length str) - (length str1)

updateDisplayList
    :: L.List FieldName ListRow
    -> E.Editor FieldName
    -> [Hc.Command]
    -> L.List FieldName ListRow
updateDisplayList l ed commands =
    L.listReplace
        (Vec.fromList
             (let words = (getUserInputWords (head (E.getEditContents ed)))
              in (commandsToRows (Hc.filterCmds words commands) words)))
        (Just 0)
        l

getUserInputWords :: String -> [String]
getUserInputWords s =
    let completed = not (null s) && (last s) == ' '
        wordList = words s
    in if completed
           then wordList
           else if null wordList
                    then []
                    else tail (reverse (words s))

commandsToRows :: [Hc.Command] -> [String] -> [ListRow]
commandsToRows commands words =
    Hc.mapCmds commands words (\tags cmd -> ListRow tags cmd)

theAttrMap :: A.AttrMap
theAttrMap =
    A.attrMap
        V.defAttr
        [ (L.listAttr, V.defAttr)
        , (L.listSelectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (customAttr, V.white `on` V.red `V.withStyle` V.defaultStyleMask)]

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"
