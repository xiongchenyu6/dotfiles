{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Control.Monad
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Actions.Volume
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Util.EZConfig         (additionalKeys)
import           XMonad.Util.Run              (spawnPipe)
import           XMonad.Util.SpawnOnce

myTerminal = "xterm"

myWorkplace = ["term","edit","web","chat","email","6:tmp"] ++ (show <$> [7..9])

myManageHook = composeAll [
    className =? "Xterm"        --> doShift "term"
  , className =? "Urxvt"        --> doShift "term"
  , className =? "Emacs"        --> doShift "edit"
  , className =? "Emacsclient"  --> doShift "edit"
  , className =? "Chromium"     --> doShift "web"
  , className =? "stalonetray"  --> doIgnore
  ]

myStartupHook = do
    setWMName "LG3D"
    spawnOnce "xterm"
    spawnOnce "emacs"
    spawnOnce "chromium"
    spawnOnce "~/.screenlayout/default.sh"

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $ desktopConfig
        { workspaces = myWorkplace
        , manageHook = manageDocks <+> myManageHook <+> manageHook def
        , layoutHook = avoidStruts  $  layoutHook def
        , terminal   = myTerminal
        , logHook    = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle  = xmobarColor "green" "" . shorten 10
                        }
        , handleEventHook = ewmhDesktopsEventHook
        , startupHook     = myStartupHook
        , modMask         = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`  customerKeyMaps


customerKeyMaps = [
          ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((mod4Mask .|. shiftMask, xK_a), spawn "sleep 0.2; scrot -s")
          -- Mute volume.
        , ((0, xF86XK_AudioMute), void toggleMute)
        -- Decrease volume.
        , ((0, xF86XK_AudioLowerVolume), void $ lowerVolume 5 )
        -- Increase volume.
        , ((0, xF86XK_AudioRaiseVolume), void $ raiseVolume 5  )
        , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10" )
        , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10" )
        , ((0, xK_Print), spawn "scrot")
        ]
