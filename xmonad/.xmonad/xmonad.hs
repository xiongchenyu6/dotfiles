{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Control.Monad
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Actions.Launcher
import           XMonad.Actions.Volume
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Prompt
import           XMonad.Prompt.Man
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Ssh
import           XMonad.Util.EZConfig         (additionalKeys)
import           XMonad.Util.Run              (spawnPipe)
import           XMonad.Util.SpawnOnce

myTerminal = "xterm"

myWorkplace = ["term","edit","web","chat","email","6:tmp"] ++ (show <$> [7..9])

myFadeHook :: Query Opacity
myFadeHook = composeAll [isUnfocused --> transparency 1, opaque]

myManageHook = composeAll [
    className =? "Xterm"        --> doShift "term"
  , className =? "Urxvt"        --> doShift "term"
  , className =? "Emacs"        --> doShift "edit"
  , className =? "Emacsclient"  --> doShift "edit"
  , className =? "Chromium"     --> doShift "web"
  , className =? "Thunderbird"     --> doShift "web"
  , className =? "stalonetray"  --> doIgnore
  ]

myStartupHook = do
    setWMName "LG3D"
    spawnOnce "xterm"
    spawnOnce "emacs"
    spawnOnce "chromium"
    spawnOnce "thurderbird"
    spawnOnce "~/.screenlayout/default.sh"

modm = mod4Mask

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $ desktopConfig
        { workspaces = myWorkplace
        , manageHook = manageDocks <+> myManageHook <+> manageHook def
        , layoutHook = avoidStruts $  myLayout
        , terminal   = myTerminal
        , logHook    = do
            dynamicLogWithPP xmobarPP
              { ppOutput = hPutStrLn xmproc
              , ppTitle  = xmobarColor "green" "" . shorten 60
              }
            fadeWindowsLogHook myFadeHook
        , handleEventHook = ewmhDesktopsEventHook
        , startupHook     = myStartupHook
        , modMask         = modm     -- Rebind Mod to the Windows key
        } `additionalKeys`  customerKeyMaps

customerKeyMaps = [
          -- Mute volume.
          ((0, xF86XK_AudioMute), void toggleMute)
        -- Decrease volume.
        , ((0, xF86XK_AudioLowerVolume), void $ lowerVolume 5 )
        -- Increase volume.
        , ((0, xF86XK_AudioRaiseVolume), void $ raiseVolume 5  )
        , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10" )
        , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10" )
        , ((mod4Mask .|. shiftMask, xK_a), spawn "sleep 0.2; scrot -s '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'")
        , ((mod4Mask .|. shiftMask, xK_p), spawn "passmenu")
        , ((mod4Mask, xK_m), manPrompt popupConfig)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_p), passGeneratePrompt popupConfig)
        , ((mod4Mask, xK_s), sshPrompt popupConfig)
        , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((modm .|. controlMask, xK_h), launcherPrompt popupConfig $ defaultLauncherModes launcherConfig)
        , ((0, xK_Print), spawn "scrot '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'")
        -- %F Equivalent to %Y-%m-%d (the ISO 8601 date format).
        -- %T The time in 24-hour notation (%H:%M:%S).
        -- $w image width
        -- $h image height
        ]

launcherConfig = LauncherConfig { pathToHoogle = "/home/chenyu/.local/bin/hoogle" , browser = "/usr/bin/chromium"}

popupConfig :: XPConfig
popupConfig = def { font = "xft:Inconsolata Nerd Font Complete:antialias=true"
                    , position = Top
                    , height = 48
                  }

myLayout = avoidStruts $
           Mirror tiled
           ||| tiled
           ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 2/100
