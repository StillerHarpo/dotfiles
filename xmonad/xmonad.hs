{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
import XMonad
import Control.Arrow(second)
import Control.Monad(unless)
import Control.Concurrent
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.SpawnOn
import System.Process
import System.IO
import System.Directory
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.SetWMName
import Control.Applicative
import Data.Default
import Data.List
import XMonad.Layout.Gaps
import XMonad.Util.Dzen
import XMonad.Util.Dmenu(menuMapArgs)
import qualified XMonad.Layout.Tabbed as T
import qualified XMonad.Layout.Groups as G
import qualified XMonad.Layout.Groups.Helpers as GH
import XMonad.Layout.Column
import qualified XMonad.Util.ExtensibleState as XS


data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

newtype ListStorage = ListStorage [WorkspaceId] deriving Typeable

instance ExtensionClass ListStorage where
  initialValue = ListStorage ["1"]

main :: IO ()
main = xmonad
     $ withUrgencyHook LibNotifyUrgencyHook
     $ ewmh def
     { terminal        ="termite"
     , modMask         = mod4Mask
     , workspaces      = map show [1 .. 20 ]
     , layoutHook      = smartBorders $ noBorders myLayout
     , manageHook      = myManageHooks
     , startupHook     = setWMName "LG3D"
     , handleEventHook = handleEventHook def <+> fullscreenEventHook
     }
     `additionalKeys`
     ([ ((0, 0x1008ff13), volume Plus)
     , ((0, 0x1008ff11),  volume Minus)
     , ((0, 0x1008ff12),  mute)
     , ((mod4Mask, 0x63), clock)
     , ((mod4Mask, xK_x), spawn toggleRedshift)
     , ((mod4Mask, xK_Return), spawn "termite")
     , ((mod4Mask .|. shiftMask, xK_Return), spawnOnEmpty "termite")
     , ((mod4Mask, xK_b), spawn "qutebrowser")
     , ((mod4Mask .|. shiftMask, xK_b), spawnOnEmpty "qutebrowser")
     , ((mod4Mask, xK_v), spawn "emacsclient -c ~/Dokumente/init.org")
     , ((mod4Mask .|. shiftMask, xK_v), spawnOnEmpty "emacsclient -c ~/Dokumente/init.org")
     , ((mod4Mask, xK_f), composeAll [actionMenu, saveFocus])
     , ((mod4Mask, xK_g), goToNotify )
     , ((mod4Mask, xK_j), GH.focusUp)
     , ((mod4Mask, xK_k), GH.focusDown)
     , ((mod4Mask, xK_h), shrinkMasterGroups)
     , ((mod4Mask, xK_l), expandMasterGroups)
     , ((mod4Mask .|. shiftMask, xK_j), GH.swapUp)
     , ((mod4Mask .|. shiftMask, xK_k), GH.swapDown)
     , ((mod4Mask, xK_s), GH.focusGroupMaster)
     , ((mod4Mask, xK_a), GH.focusGroupUp)
     , ((mod4Mask, xK_d), GH.focusGroupDown)
     , ((mod4Mask .|. shiftMask, xK_a), GH.moveToGroupUp False)
     , ((mod4Mask .|. shiftMask, xK_d), GH.moveToGroupDown False)
 ]
     ++ [((mod4Mask, k), composeAll
        [windows .  W.greedyView $ show i, saveFocus])
        | (i, k) <- zip [1 ..9] [xK_1 .. xK_9]]
    )
    `additionalKeysP`
    [ ("<XF86AudioPlay>", spawn $ dbus ++ dest ++ org ++ "PlayPause")
    , ("<XF86AudioPrev>", spawn $ dbus ++ dest ++ org ++ "Previous")
    , ("<XF86AudioNext>", spawn $ dbus ++ dest ++ org ++ "Next")]
      where
              spawnOnEmpty prog = composeAll [viewEmptyWorkspace, spawn prog, saveFocus]
              listWindows= "termite -e ~/projects/python/listWindows/listWindows.py"
              dbus = "dbus-send --print-reply "
              dest = "--dest=org.mpris.MediaPlayer2.spotify "
              org = "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player."
              toggleRedshift = "systemctl --user is-active redshift.service"
                              ++ " && systemctl --user stop redshift.service"
                              ++ " || systemctl --user start redshift.service"

-- Key scripts --
myDzenConfig :: Int -> String -> X ()
myDzenConfig len = dzenConfig (timeout 1 >=> centered)
  where centered = onCurr (center len 66)
             >=> font "-adobe-helvetica-*-*-*-*-24-*-*-*-*-*-*-*"
             >=> addArgs ["-fg", "#10bb20"]

clock :: X ()
clock = output >>= myDzenConfig 600
  where
   output = do date <- runProcessWithInput "date" ["+%H:%M:%S%n%A, %d. %B %Y"] []
               battery <- io $ readFile "/sys/class/power_supply/BAT0/capacity"
               pure . replace $ date ++ "     Battery: " ++ init battery ++ "%"
   replace "" = ""
   replace ('\n':str) = ' ' : replace str
   replace (s:str) = s : replace str

mute :: X ()
mute = script >> output >>= myDzenConfig 300
  where
    script = runProcessWithInput "pactl" [ "set-sink-mute", "@DEFAULT_SINK@", "toggle" ] ""
    output = head . filter (isInfixOf "muted") . take 1000 . dropWhile (isInfixOf "*"). lines
      <$> runProcessWithInput "pacmd" ["list-sinks"] ""


data Volume = Plus | Minus
volume :: Volume -> X ()
volume vol = script >> output >>= myDzenConfig 300
  where
    script = runProcessWithInput "pactl" [ "set-sink-volume", "@DEFAULT_SINK@", volChar vol ++ "5%" ] ""
    volChar Plus = "+"
    volChar Minus = "-"
    output = (!! 4) . words . head . filter (isInfixOf "volume")
      . take 1000 . dropWhile (isInfixOf "*") . lines
      <$> runProcessWithInput "pacmd" ["list-sinks"] ""

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
    [ isFullscreen --> doFullFloat
    , manageSpawn <+> manageHook def
    , className =? "mpv" --> doFullFloat
    , className =? "feh" --> doFullFloat
    ]

myLayout = Full ||| shrinked ||| tapped
  where shrinked = gaps [(L,300), (R,300)] Full
        tapped   = G.group T.simpleTabbed $ Mirror $ Column 1

-- | programms that can be start from dmenu
programms :: [(String,String)] -- ^ (name in dmenu, executable)
programms = [ ("firefox","firefox")
            , ("anki", "anki")
            , ("signal", "signal-desktop")
            , ("spotify", "spotify")
            , ("netflix", "google-chrome-stable \"netflix.com\"")
            , ("mutt", startTerm "mutt")
            , ("calcurse", startTerm "calcurse")
            , ("toxic", startTerm "toxic")
            , ("rtv", startTerm "rtv")
            , ("newsboat", startTerm "newsboat")
            , ("tor", "tor-browser")
            , ("termite", "termite")
            ]
  where
    startTerm s = "termite --title=" ++ s ++ " --exec=" ++ s

-- | save the current workspace, so you can later come back with goToNotify
saveFocus :: X()
saveFocus = do
   i <- W.tag . W.workspace . W.current <$> gets windowset
   XS.modify $ changeHead i

changeHead :: WorkspaceId -> ListStorage -> ListStorage
changeHead _ l@(ListStorage [])   = l
changeHead i (ListStorage (_:xs)) = ListStorage $ i:xs

-- | cycle through the windows that triggered urgency and go back to the window,
--   you came from
goToNotify :: X()
goToNotify =  do
       i <- lastStorage <$> XS.get
       XS.modify deleteN
       windows $ W.greedyView i

lastStorage :: ListStorage -> WorkspaceId
lastStorage (ListStorage xs) = last xs

deleteN :: ListStorage -> ListStorage
deleteN l@(ListStorage [x]) = l
deleteN (ListStorage xs)    = ListStorage $ init xs

-- | if a window triggers a urgency, save the workspace on which it is
--   and show a notification
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        unless (match $ show name) $ do
          Just idx <- W.findTag w <$> gets windowset
          safeSpawn "notify-send" ["-t 3000", show name, "workspace " ++ idx]
          XS.modify $ addCont idx
        where
          unwantedNotifcations = [ "qutebrowser" ]
          match n = any (matchEnd n) unwantedNotifcations
          matchEnd n uN = take (length uN) (reverse n) == reverse uN


-- | add the workspace at the end of the list, if it isn't already in it
addCont :: WorkspaceId -> ListStorage -> ListStorage
addCont i l@(ListStorage xs) | i `elem` xs = l
                             | otherwise   = ListStorage $ xs ++ [i]


-- | Calls dmenuMap to grab the appropriate Window, and hands it off to action
--   if found.
actionMenu :: X ()
actionMenu = windowMap >>= menuMapFunction >>= flip whenJust id
    where
      menuMapFunction :: M.Map String a -> X (Maybe a)
      menuMapFunction = menuMapArgs "dmenu" ["-i","-l","30"]

-- | A map from window names to Windows, given a windowTitler function.
windowMap :: X (M.Map String (X ()))
windowMap = do
  ws <- gets windowset
  M.fromList . (++ openProgramms) . toAction . concat
    <$> mapM keyValuePairs (W.workspaces ws)
 where keyValuePairs ws = mapM (keyValuePair ws) $ W.integrate' (W.stack ws)
       keyValuePair ws w = (, w) <$> decorateName ws w
       toAction = map (second (windows . W.focusWindow))
       openProgramms = map
                         (\(name,prog)
                           -> ("𞹾" ++ name,
                              composeAll [viewEmptyWorkspace,
                                           spawn prog,saveFocus]))
                         programms


-- | Returns the window name as will be listed in dmenu.
--   Tagged with the workspace ID, to guarantee uniqueness, and to let the user
--   know where he's going.
decorateName :: WindowSpace -> Window -> X String
decorateName ws w = do
  name <- show <$> getName w
  return $ name ++ " [" ++ W.tag ws ++ "]"
