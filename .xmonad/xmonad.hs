import XMonad
import System.Exit
import System.IO

import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.NoBorders

import XMonad.Util.Dmenu
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Scratchpad
import XMonad.Util.NamedWindows

import Control.Monad
import Graphics.X11.ExtraTypes.XF86

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    ws <- gets windowset
    whenJust (W.findTag w ws) (flash name)
      where flash name index = safeSpawn "notify-send" [index ++ ": " ++ show name]

myTerminal           = "urxvt"
myBorderWidth        = 1
myNormalBorderColor  = "snow4"
myFocusedBorderColor = "#61ce3c"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myManageHook = manageDocks
    <+> composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , isFullscreen --> doFullFloat
    ]
    <+> manageHook defaultConfig

myStartupHook = do
    safeSpawn "synergys" []
    safeSpawn "xautolock" ["-locker", "gnome-screensaver-command --lock", "-time", "10"]
    unsafeSpawn "$HOME/.filesystem/bin/Gwall"

myLayout = avoidStruts
    $ smartBorders
    $ layoutHook defaultConfig

quitWithWarning :: X()
quitWithWarning = do
    let m = "confirm quit"
    s <- dmenu [m]
    when (m == s) (io exitSuccess)

myXmobarPP h = xmobarPP
        { ppOutput = hPutStrLn h
        , ppCurrent = xmobarColor "#f8f8f8" "DodgerBlue4" . wrap " " " "
        , ppVisible = xmobarColor "#f8f8f8" "LightSkyBlue4" . wrap " " " "
        , ppUrgent = xmobarColor "#f8f8f8" "red4" . wrap " " " " . xmobarStrip
        , ppLayout = wrap " |" "" . xmobarColor "DarkOrange" "" . wrap " [" "] "
        , ppTitle = xmobarColor "#61ce3c" "" . shorten 50
        , ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByIndex
        , ppSep = ""
        , ppWsSep = " | "
        }

main = do
    xmobar <- spawnPipe "/usr/bin/xmobar $HOME/.xmonad/xmobarrc"
    xmonad $ withUrgencyHook LibNotifyUrgencyHook defaultConfig
        { manageHook                 = myManageHook
        , layoutHook                 = myLayout
        , handleEventHook            = fullscreenEventHook
        , focusFollowsMouse          = myFocusFollowsMouse
        , logHook                    = (dynamicLogWithPP $ myXmobarPP xmobar)
        , modMask                    = mod4Mask     -- Rebind Mod to the Windows key
        , terminal                   = myTerminal
        , startupHook                = myStartupHook
        , borderWidth                = myBorderWidth
        , normalBorderColor          = myNormalBorderColor
        , focusedBorderColor         = myFocusedBorderColor
        }
        `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "/usr/bin/gnome-screensaver-command -l")
        , ((mod4Mask .|. shiftMask, xK_o), spawn "/usr/bin/fetchotp -x")
        , ((mod4Mask .|. shiftMask, xK_b), spawn "/usr/bin/google-chrome")
        , ((mod4Mask .|. shiftMask, xK_q), quitWithWarning)
        , ((mod4Mask, xK_b), sendMessage ToggleStruts)
        , ((0, xF86XK_AudioLowerVolume), spawn "/usr/bin/amixer set Master 2dB-")
        , ((0, xF86XK_AudioRaiseVolume), spawn "/usr/bin/amixer set Master 2dB+")
        , ((0, xF86XK_AudioMute), spawn "/usr/bin/amixer set Master toggle")
        , ((controlMask, xK_Print), spawn "sleep 0.2; /usr/bin/scrot -s")
        , ((0, xK_Print), spawn "/usr/bin/scrot")
        ]
