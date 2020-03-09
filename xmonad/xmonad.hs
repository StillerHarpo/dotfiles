{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Protolude
import qualified Data.Text as T


import           Control.Applicative
import           Control.Concurrent
import           Control.Monad (unless, void, join, when)
import           Data.Default
import           Data.List (last)
import qualified Data.Map as M
import           System.Directory
import           System.Process
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.FindEmptyWorkspace
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Column
import           XMonad.Layout.Gaps
import qualified XMonad.Layout.Groups as G
import qualified XMonad.Layout.Groups.Helpers as GH
import           XMonad.Layout.NoBorders
import qualified XMonad.Layout.Tabbed as T
import           XMonad.Prompt hiding (font)
import           XMonad.Prompt.RunOrRaise
import qualified XMonad.StackSet as W
import           XMonad.Util.Dmenu (menuArgs)
import           XMonad.Util.Dzen
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run
import           XMonad.Util.Stack
import           XMonad.Actions.MessageFeedback
import           Data.Maybe(fromMaybe)

import           Bookmarks

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

newtype ListStorage = ListStorage [Window] deriving Typeable

instance ExtensionClass ListStorage where
  initialValue = ListStorage []

main :: IO ()
main = xmonad
     $ withUrgencyHook LibNotifyUrgencyHook
     $ ewmh def
     { terminal        = "alacritty"
     , modMask         = mod4Mask
     , workspaces      = map show [1 .. 20 ]
     , layoutHook      = smartBorders $ noBorders myLayout
     , logHook         = updatePointer (0.5, 0.5) (0, 0)
     , manageHook      = myManageHooks
     , startupHook     = setWMName "LG3D" >> spawn "feh --bg-scale ~/black.png"
                                          >> spawn "firefox"
     , handleEventHook = handleEventHook def <+> fullscreenEventHook
     }
     `additionalKeys`
     ([ ((0                     , 0x1008ff13), volume Plus)
      , ((0                     , 0x1008ff11), volume Minus)
      , ((0                     , 0x1008ff12), mute)
      , ((mod4Mask              , 0x63)      , clock)
      , ((mod4Mask              , xK_x)      , spawn toggleRedshift)
      , ((mod4Mask              , xK_y)      , spawn toggleMonitor)
      , ((mod4Mask              , xK_Return) , spawn        "alacritty")
      , ((mod4Mask .|. shiftMask, xK_Return) , spawnOnEmpty "alacritty")
      , ((mod4Mask              , xK_v)      , spawn        emacs)
      , ((mod4Mask .|. shiftMask, xK_v)      , spawnOnEmpty emacs)
      , ((mod4Mask              , xK_n)      , spawn "networkmanager_dmenu")
      , ((mod4Mask              , xK_f)      , composeAll [ runOrShift
                                                          , saveFocus])
      , ((mod4Mask .|. shiftMask, xK_f)      , composeAll [runOrRaise
                                                          , saveFocus])
      , ((mod4Mask              , xK_g)      , goToNotify )
      , ((mod4Mask              , xK_j)      , GH.focusUp)
      , ((mod4Mask              , xK_k)      , GH.focusDown)
      , ((mod4Mask              , xK_h)      , shrinkMasterGroups)
      , ((mod4Mask              , xK_l)      , expandMasterGroups)
      , ((mod4Mask .|. shiftMask, xK_j)      , GH.swapUp)
      , ((mod4Mask .|. shiftMask, xK_k)      , GH.swapDown)
      , ((mod4Mask              , xK_s)      , GH.focusGroupMaster)
      , ((mod4Mask              , xK_a)      , GH.focusGroupUp)
      , ((mod4Mask              , xK_d)      , GH.focusGroupDown)
      , ((mod4Mask .|. shiftMask, xK_a)      , GH.moveToGroupUp False)
      , ((mod4Mask .|. shiftMask, xK_d)      , GH.moveToGroupDown False)
      , ((mod4Mask              , xK_p)      , playPause)
      , ((mod4Mask              , xK_i)      , playPrevious)
      , ((mod4Mask              , xK_o)      , playNext)
      -- rematch workspace keys to take workspaces in to account
      , ((mod4Mask              , xK_w)      , composeAll [ nextScreen
                                                          , saveFocus])
      , ((mod4Mask              , xK_e)      , composeAll [ swapNextScreen
                                                          , saveFocus])
      ]
     ++
      [ ((mod4Mask              , k)         , saveView i)
                  | (i, k) <- zip [1 ..9] [xK_1 .. xK_9]
      ]
    )
    `additionalKeysP`
    [( "<XF86MonBrightnessUp>"  , backlight Plus)
    , ("<XF86MonBrightnessDown>", backlight Minus)
    , ("<XF86AudioPlay>"        , playPause)
    , ("<XF86AudioPrev>"        , playPrevious)
    , ("<XF86AudioNext>"        , playNext)]
      where
        spawnOnEmpty prog = composeAll [viewEmptyWorkspace, spawn prog, saveFocus]
        saveView i = composeAll [windows . W.greedyView $ show i , saveFocus]
        listWindows = "alacritty -e ~/projects/python/listWindows/listWindows.py"
        emacs = "emacsclient -c ~/Dokumente/init.org"
        dbus = "dbus-send --print-reply "
        dest = "--dest=org.mpris.MediaPlayer2.spotify "
        org = "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player."
        playPause = spawn $ dbus ++ dest ++ org ++ "PlayPause"
        playNext = spawn $ dbus ++ dest ++ org ++ "Next"
        playPrevious = spawn $ dbus ++ dest ++ org ++ "Previous"
        toggleRedshift = "systemctl --user is-active redshift.service"
                        ++ " && systemctl --user stop redshift.service"
                        ++ " || systemctl --user start redshift.service"
        toggleMonitor = "~/scripts/toggleMonitor"


-- Key scripts --
myDzenConfig :: Int -> Text -> X ()
myDzenConfig len = dzenConfig (timeout 1 >=> centered) . T.unpack
  where centered = onCurr (center len 66)
             >=> font "-adobe-helvetica-*-*-*-*-24-*-*-*-*-*-*-*"
             >=> addArgs ["-fg", "#10bb20"]

clockText :: X Text
clockText = do
  date <- runProcessWithInput "date" ["+%H:%M:%S%n%A, %d. %B %Y"] []
  battery <- io $ readFile "/sys/class/power_supply/BAT0/capacity"
  pure . T.replace "\n" " " $ T.pack date `T.append` "     Battery: " `T.append` T.init battery `T.append` "%"

clock :: X ()
clock = clockText >>= myDzenConfig 600

mute :: X ()
mute = script >> output >>= myDzenConfig 300
  where
    script = runProcessWithInput "pactl" [ "set-sink-mute", "@DEFAULT_SINK@", "toggle" ] ""
    output = fromMaybe "command failed" .
      head . filter (T.isInfixOf "Mute:") . dropWhile (T.isInfixOf "State: RUNNING"). T.lines . T.pack
      <$> runProcessWithInput "pactl" ["list", "sinks"] ""


data Direction = Plus | Minus

volume :: Direction -> X ()
volume vol = script >> output >>= myDzenConfig 300
  where
    script = runProcessWithInput "pactl" [ "set-sink-volume", "@DEFAULT_SINK@", volChar vol ++ "5%" ] ""
    volChar Plus = "+"
    volChar Minus = "-"
    output = fromMaybe "command failed"
           . ((`atMay` 4) <=< fmap T.words
           . head . filter (T.isInfixOf "Volume:")
           . dropWhile (T.isInfixOf "State: RUNNING") . T.lines . T.pack)
           <$> runProcessWithInput "pactl" ["list", "sinks"] ""

backlight :: Direction -> X ()
backlight dir = void $ runProcessWithInput "light" [ dirOp dir, "10" ] ""
  where
    dirOp Plus = "-A"
    dirOp Minus = "-U"

-- Dont work use instead m,n
-- 0x1008ff02 = XFMonBrightnessUp
-- 0x1008ff03 = XFMonBrightnessDown

-- | Shrink the master area
shrinkMasterGroups :: X ()
shrinkMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage Shrink

-- | Expand the master area
expandMasterGroups :: X ()
expandMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage Expand

myManageHooks = composeAll
    [ manageSpawn <+> manageHook def
    , doSink
    ]

doSink :: ManageHook
doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)

myLayout = Full ||| shrinked ||| tabbed
  where shrinked = gaps [(L,300), (R,300)] Full
        -- tabbed   = G.group T.simpleTabbed $ Mirror $ Column 1
        tabbed   = G.group Full $ Mirror $ Column 1

-- | programms that can be started from rofi
programms :: [(Text,Text)] -- ^ (name in dmenu, executable)
programms = [ ("firefox"            , "firefox")
            , ("anki"               , "anki")
            , ("signal"             , "signal-desktop")
            , ("mattermost-desktop" , "mattermost-desktop")
            , ("spotify"            , "spotify")
            , ("steam"              , "steam")
            , ("youtube"            , startTerm "youtube-viewer")
            , ("mu4e"               , emacs "mu4e")
            , ("elfeed"             , emacs "elfeed")
            , ("calcurse"           , startTerm "calcurse")
            , ("toxic"              , startTerm "toxic")
            , ("rtv"                , startTerm "rtv")
            , ("tor"                , "tor-browser")
            , ("alacritty"          , "alacritty")
            , ("virtualBox"         , "VirtualBox")
            ]
            ++ map (second browser) bookmarks
  where
    startTerm s = "alacritty -t \"" `T.append` s `T.append` "\" -e \""
                  `T.append` s `T.append` "\""
    emacs s = "emacsclient -c -e \"(" `T.append` s `T.append` ")\""
              `T.append` maximizeEmacs
    maximizeEmacs = " -e \"(spacemacs/toggle-maximize-buffer)\""

-- | save the current window, so you can later come back with goToNotify
saveFocus :: X()
saveFocus = do
   workspace <- W.stack . W.workspace . W.current <$> gets windowset
   let window = W.focus <$> workspace
   whenJust window
     (XS.modify . changeHead)

changeHead :: Window -> ListStorage -> ListStorage
changeHead _ l@(ListStorage [])   = l
changeHead w (ListStorage (_:ws)) = ListStorage $ w:ws

-- | cycle through the windows that triggered urgency and go back to the window,
--   you came from
goToNotify :: X()
goToNotify =  do
       w <- lastStorage <$> XS.get
       XS.modify deleteN
       windows $ W.focusWindow w

lastStorage :: ListStorage -> Window
lastStorage (ListStorage xs) = last xs

deleteN :: ListStorage -> ListStorage
deleteN l@(ListStorage [x]) = l
deleteN (ListStorage xs)    = ListStorage $ initSafe xs

-- | if a window triggers a urgency, save the workspace on which it is
--   and show a notification
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        unless (match $ show name) $ do
          Just idx <- W.findTag w <$> gets windowset
          safeSpawn "notify-send" ["-t 3000", show name, "workspace " ++ idx]
          XS.modify $ addCont w
        where
          unwantedNotifcations = [ "qutebrowser" ]
          match n = any (matchEnd n) unwantedNotifcations
          matchEnd n uN = take (length uN) (reverse n) == reverse uN

-- | add the workspace at the end of the list, if it isn't already in it
addCont :: Window -> ListStorage -> ListStorage
addCont w l@(ListStorage ws) | w `elem` ws = l
                             | otherwise   = ListStorage $ ws ++ [w]

-- | open url in browser
browser ::  Text -> Text
browser url = "firefox -new-window \"" `T.append` urlWithSearch `T.append` "\""
  where
   urlWithSearch = if T.any (== '.') url && not (T.any (== ' ')  url)
                   then url
                   else "duckduckgo.com\\?q=" `T.append` replaceSpaces url

-- | Calls dmenuMap to grab the appropriate Window or program, and hands it off to action
--   if found .
actionMenu :: (Window -> X()) -- ^ the action
           -> Bool -- ^ should the program be open on a new workspace
           -> X ()
actionMenu action viewEmpty = do
  time <- clockText
  let menuMapFunction =
        menuMapArgsDefault "rofi"
                           [ "-dmenu"
                           , "-i"
                           , "-p"
                           , "window"
                           , "-theme"
                           , "gruvbox-dark"
                           , "-mesg"
                           , time
                           , "-fullscreen"]
                           defaultAction
  join (windowMap >>= menuMapFunction)
    where
      defaultAction url = when (T.length url > 0) $ composeAll $ [ viewEmptyWorkspace | viewEmpty ]
                                                               ++ [spawn . T.unpack $ browser url]
      -- | A map from window names to Windows, given a windowTitler function.
      windowMap :: X (M.Map Text (X ()))
      windowMap = do
        ws <- gets $ W.workspaces . windowset
        M.fromList . (++ openProgramms) . toAction . concat
          <$> mapM keyValuePairs ws
       where keyValuePairs ws = mapM (keyValuePair ws) $ W.integrate' (W.stack ws)
             keyValuePair ws w = (, w) <$> decorateName ws w
             toAction = map $ second action
             openProgramms = map
                               (\(name,prog)
                                 -> (name,
                                    composeAll ([viewEmptyWorkspace | viewEmpty]
                                                ++ [spawn (T.unpack prog), saveFocus])))
                               programms

runOrRaise :: X ()
runOrRaise = actionMenu (windows . W.focusWindow) True

runOrShift :: X ()
runOrShift = actionMenu action False
  where action w = do
          idx <- W.currentTag <$> gets windowset
          windows $ delWin idx w
          windows $ W.insertUp w
          insertUp w

-- | Returns the window name as will be listed in dmenu.
--   Tagged with the workspace ID, to guarantee uniqueness, and to let
--   the user know where they going.
decorateName :: WindowSpace -> Window -> X Text
decorateName ws w = do
  name <-  resClass <$> withDisplay (\d -> io $ getClassHint d w)
  displayName <- show <$> getName w
  return $ T.pack name `T.append` " - " `T.append` displayName
           `T.append` " [" `T.append` T.pack (W.tag ws) `T.append` "]"


-- | temporally deletes a window on a workspace
delWin :: (Ord a, Eq s, Eq i) => i -> a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
delWin n w s = case W.findTag w s of
                    Just from | n `W.tagMember` s -> go from s
                    _                             -> s
    where go from = onWorkspace from (W.delete' w)

-- | Like 'menuMapArgs' but also takes a default command
menuMapArgsDefault :: MonadIO m => Text -> [Text]
                   -> (Text -> a)
                   -> M.Map Text a
                   -> m a
menuMapArgsDefault menuCmd args def selectionMap = do
  selection <- menuFunction . map T.unpack $ M.keys selectionMap
  return $ fromMaybe (def selection) $ M.lookup selection selectionMap
      where
        menuFunction = fmap T.pack . menuArgs (T.unpack menuCmd) (map T.unpack args)

-- | is not exported from Stackset
onWorkspace :: (Eq i, Eq s) => i -> (W.StackSet i l a s sd -> W.StackSet i l a s sd)
            -> (W.StackSet i l a s sd -> W.StackSet i l a s sd)
onWorkspace n f s = W.view (W.currentTag s) . f . W.view n $ s

-- | inserts a window over the currently focused one
insertUp w = alt (onFocused $ insertUpZ w) (W.insertUp w)

-- | Group helpers
onFocused :: (Zipper Window -> Zipper Window) -> G.ModifySpec
onFocused f _ gs = onFocusedZ (G.onZipper f) gs

alt :: G.ModifySpec -> (WindowSet -> WindowSet) -> X ()
alt f g = alt2 (G.Modify f) $ windows g

alt2 :: G.GroupsMessage -> X () -> X ()
alt2 m x = do b <- send m
              unless b x

replaceSpaces :: Text -> Text
replaceSpaces = T.map (\case
                          ' ' -> '+'
                          c -> c) . T.strip
