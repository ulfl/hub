-- Copyright (C) 2016-2019 Ulf Leopold
--
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hub.Hub
  ( hub
  ) where

import Brick.AttrMap (AttrMap, AttrName, attrMap)
import Brick.Main (App(..), continue, defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent(..), EventM, Next, Padding(..))
import Brick.Types (Widget)
import Brick.Util (on)
import Brick.Widgets.Core
  ( fill
  , hBox
  , hLimit
  , padLeft
  , padRight
  , str
  , vBox
  , vLimit
  , withAttr
  )
import Brick.Widgets.Edit
  ( Editor
  , applyEdit
  , editor
  , getEditContents
  , handleEditorEvent
  , renderEditor
  )
import Brick.Widgets.List
  ( List
  , list
  , listAttr
  , listElementsL
  , listMoveDown
  , listMoveUp
  , listReplace
  , listSelectedAttr
  , listSelectedElement
  , renderList
  )
import Control.Lens ((^.))
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Text.Zipper (deletePrevChar)
import Data.Vector (fromList)
import Graphics.Vty
  ( Event(..)
  , Key(..)
  , Modifier(..)
  , defAttr
  , defaultStyleMask
  , red
  , reverseVideo
  , white
  , withStyle
  )
import Hub.CmdLine
import Hub.CommandType
  ( Command
  , filterCmdsAndTags
  , getShellCmd
  , isPartialMatchTag
  , mapCmds
  , toStdout
  )
import Hub.Config (readConfig)
import System.Exit (exitSuccess)
import System.Process (callCommand)
import Text.Printf (printf)

data FieldName
  = ListField
  | EditField
  deriving (Ord, Show, Eq)

data State =
  State (List FieldName ListRow) -- The list widget.
        (Editor String FieldName) -- The editor widget.
        [Command] -- List of available 'Commands'.
        Action -- Action to take on exit.

data ListRow =
  ListRow String
          String
  deriving (Ord, Show, Eq)

type Cmd = String

data Action
  = Run Cmd
  | Print Cmd
  | JustExit

hub :: IO ()
hub = do
  appCfg <- getAppConfig
  cmds <- readConfig appCfg
  let filteredCmds = filterCmdsAndTags (tags appCfg) cmds
  when
    (stdOut appCfg)
    (do mapM_ toStdout filteredCmds
        exitSuccess :: IO ())
  cmd <-
    case filteredCmds of
      [cmd] -> return (Run (getShellCmd cmd))
      _ -> do
        State _ _ _ cmd <- defaultMain theApp (initialState filteredCmds)
        return cmd
  action appCfg cmd

action :: AppConfig -> Action -> IO ()
action appCfg (Run cmd) =
  if not (dryrun appCfg)
    then callCommand cmd
    else putStrLn cmd
action _ (Print cmd) = putStrLn cmd
action _ JustExit = return ()

-- Internal ============================================================
initialState :: [Command] -> State
initialState cmds =
  State
    (list ListField (fromList (commandsToRows cmds)) 2)
    (editor EditField (Just 1) "")
    cmds
    JustExit

theApp :: App State Graphics.Vty.Event FieldName
theApp =
  App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = return
    , appAttrMap = const theAttrMap
    }

drawUI :: State -> [Widget FieldName]
drawUI state = [ui]
  where
    State l e cmds _ = state
    box = renderList listDrawElement False l
    prompt = renderEditor (str . unlines) True e
    ui =
      vBox
        [ box
        , hBox
            [ withAttr
                listSelectedAttr
                (str
                   (Text.Printf.printf
                      "Showing %d of %d items."
                      (length (l ^. listElementsL))
                      (length cmds)))
            , withAttr listSelectedAttr (vLimit 1 (fill ' '))
            ]
        , hBox [str "hub> ", prompt]
        ]

listDrawElement :: Bool -> ListRow -> Widget FieldName
listDrawElement sel (ListRow tags description) =
  let attr =
        if sel
          then customAttr
          else listAttr
   in hBox
        [ withAttr attr (vLimit 2 (hLimit 1 (fill ' ')))
        , vLimit 2 (hLimit 1 (fill ' '))
        , padRight Max (vBox [str tags, padLeft (Pad 8) (str description)])
        ]

appEvent :: State -> BrickEvent FieldName e -> EventM FieldName (Next State)
appEvent (State l ed commands action) be =
  case be of
    VtyEvent e ->
      case e of
        EvKey (KChar 'n') [MCtrl] ->
          continue (State (listMoveDown l) ed commands action)
        EvKey KDown [] -> continue (State (listMoveDown l) ed commands action)
        EvKey (KChar 'p') [MCtrl] ->
          continue (State (listMoveUp l) ed commands action)
        EvKey KUp [] -> continue (State (listMoveUp l) ed commands action)
        EvKey (KChar 'w') [MCtrl] ->
          let words = head (getEditContents ed)
              len = lengthOfPrevWord words
              ed2 = foldl (\a _ -> applyEdit deletePrevChar a) ed [1 .. len]
              l2 = updateDisplayList l ed2 commands
           in continue (State l2 ed2 commands action)
        EvKey KEnter [] ->
          let action =
                case listSelectedElement l of
                  Just (_, ListRow _ cmd) -> Run cmd
                  Nothing -> JustExit
           in halt (State l ed commands action)
        EvKey KEsc [] ->
          let action =
                case listSelectedElement l of
                  Just (_, ListRow _ cmd) -> Print cmd
                  Nothing -> JustExit
           in halt (State l ed commands action)
        EvKey (KChar 'c') [MCtrl] -> halt (State l ed commands JustExit)
        _ -> do
          ed2 <- handleEditorEvent e ed
          let l2 = updateDisplayList l ed2 commands
          continue (State l2 ed2 commands action)
    _ -> continue (State l ed commands action)

lengthOfPrevWord :: String -> Int
lengthOfPrevWord str =
  let str1 =
        dropWhile
          (not . Data.Char.isSpace)
          (dropWhile Data.Char.isSpace (reverse str))
   in length str - length str1

updateDisplayList ::
     List FieldName ListRow
  -> Editor String FieldName
  -> [Command]
  -> List FieldName ListRow
updateDisplayList l ed commands =
  listReplace
    (fromList
       (let words = getUserInputWords (head (getEditContents ed))
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
commandsToRows commands = mapCmds commands ListRow

theAttrMap :: AttrMap
theAttrMap =
  attrMap
    defAttr
    [ (listAttr, defAttr)
    , (listSelectedAttr, defAttr `withStyle` reverseVideo)
    , (customAttr, white `on` red `withStyle` defaultStyleMask)
    ]

customAttr :: AttrName
customAttr = listSelectedAttr <> "custom"
