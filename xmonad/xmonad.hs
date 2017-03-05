import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
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

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- W.findTag w Control.Applicative.<$> gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]
        liftIO ( do  
             filePath <- liftIO $ fmap (++ "/scripts/var/notifyWindows") getHomeDirectory
             handle <- openFile filePath ReadMode
             (tempName, tempHandle) <- openTempFile "~/scripts/var" "tempHaskell"
             contents <- hGetContents handle
             hPutStr  tempHandle $  show $ checkCon (read idx) (getCont contents) 
             hClose handle
             hClose tempHandle
             removeFile filePath
             renameFile tempName filePath
             )
 
checkCon :: Int -> [Int] -> [Int]
checkCon i xs | i `elem` xs = xs
              | otherwise = xs ++ [i]  

getCont :: String -> [Int]
getCont "" = [1]
getCont c = read c


main :: IO ()
main = xmonad
     $ withUrgencyHook LibNotifyUrgencyHook
     $ ewmh def
     { terminal        ="termite"
     , modMask         = mod4Mask
     , workspaces      = map show [1 .. 10 ]
     , layoutHook      = smartBorders myLayout
     , startupHook     = startup
     , manageHook      = myManageHooks
     , handleEventHook = handleEventHook def <+> fullscreenEventHook
     } 
     `additionalKeys` 
     ([ ((0, 0x1008ff13),  spawn "~/scripts/volumeplus")
     , ((0, 0x1008ff11),  spawn "~/scripts/volumeminus")
     , ((0, 0x1008ff12),  spawn "~/scripts/mute")
     , ((mod4Mask, 0x63), spawn "~/scripts/clock")
     , ((mod4Mask, xK_a), spawn "systemctl hibernate")
     , ((mod4Mask, xK_f), spawnAndDo doFullFloat "termite -e ~/projects/python/listWindows/listWindows.py")
     , ((mod4Mask, xK_s), goToNotify ) 
 ]
     ++ [((mod4Mask, k), composeAll [windows .  W.greedyView $ show i, saveFocus i])
        | (i, k) <- zip [1 ..9] [xK_1 .. xK_9]]
    )
    `additionalKeysP`
    [ ("<XF86AudioPlay>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ("<XF86AudioPrev>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    , ("<XF86AudioNext>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")]

    

-- Dont work use instead m,n
-- 0x1008ff02 = XFMonBrightnessUp
-- 0x1008ff03 = XFMonBrightnessDown

 
myManageHooks = composeAll
    [ isFullscreen --> doFullFloat
    , manageSpawn <+> manageHook def
    , className =? "mpv" --> doFullFloat
    , className =? "feh" --> doFullFloat
    ]

myLayout = Full ||| tiled ||| Mirror tiled 
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

startup :: X()
startup = do
  setWMName "LG3D"
  spawnOn "1" "termite -e calcurse"
  spawnOn "3" "termite -e toxic"
  spawnOn "10" "firefox -P DarkFirefox"
  spawnOn "10" "firefox -P BrightFirefox"
  spawnOn "4" "firefox"
  spawnOn "2" "termite -e mutt"
  spawn "xrandr --output DVI-I-1 --off"
  path <- liftIO $ fmap (++ "/scripts/var/notifyWindows") getHomeDirectory
  liftIO $ writeFile path "[1]"
  liftIO $ writeFile "~/scripts/var/hddoff" "1"

saveFocus :: Int -> X()
saveFocus i = do 
   (filePath, handle, tempName, tempHandle, contents) <- getTempFile
   liftIO (hPutStr  tempHandle $  show ( i : tail (getCont contents)))
   closeTempFile filePath handle tempName tempHandle

goToNotify :: X()
goToNotify =  do 
       (filePath, handle, tempName, tempHandle, contents) <- getTempFile
       windows $ W.greedyView $ show $ last' $ read contents
       liftIO $ hPutStr  tempHandle $  show $ deleteN $ read contents
       closeTempFile filePath handle tempName tempHandle

getTempFile :: X (String, Handle , String, Handle, String)
getTempFile = do
       path <- liftIO $ fmap (++ "/scripts/var") getHomeDirectory
       let filePath = path ++ "/notifyWindows"
       handle <- liftIO $ openFile filePath ReadMode
       (tempName, tempHandle) <- liftIO $ openTempFile path "tempHaskell"
       contents <- liftIO $ hGetContents handle 
       return(filePath, handle, tempName, tempHandle, contents)

closeTempFile :: String -> Handle -> String -> Handle -> X()
closeTempFile filePath handle tempName tempHandle = do
       liftIO $ hClose handle
       liftIO $ hClose tempHandle
       liftIO $ removeFile filePath
       liftIO $ renameFile tempName filePath 
       

deleteN :: [Int] -> [Int]
deleteN [x] = [x]
deleteN xs = init xs

last' :: [Int] -> Int
last' [x] = x
last' (x:xs) = last' xs
