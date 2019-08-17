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
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Prompt
import           XMonad.Prompt.Man
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Ssh
import           XMonad.Util.EZConfig           ( additionalKeys )
import           XMonad.Util.Run                ( spawnPipe )
import           XMonad.Util.SpawnOnce
import           XMonad.Wallpaper


myTerminal :: String
myTerminal = "kitty"

myWorkplace :: [String]
myWorkplace =
  ["term", "edit", "web", "chat", "email", "6:tmp"] ++ (show <$> [7 .. 9])

myManageHook = composeAll
  [ className =? "Xterm" --> doShift "term"
  , className =? "Urxvt" --> doShift "term"
  , className =? "Kitty" --> doShift "term"
  , className =? "kitty" --> doShift "term"
  , className =? "Chromium" --> doShift "web"
  , className =? "Thunderbird" --> doShift "email"
  , className =? "Wechat" --> doShift "chat"
  , className =? "Conky-manager" --> doShift "chat"
  , className =? "Conky" --> doShift "chat"
  , className =? "conky" --> doShift "chat"
  , className =? "Wechat" --> doShift "chat"
  , className =? "wechat" --> doShift "chat"
  , className =? "stalonetray" --> doIgnore
  ]

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawnOnce "stalonetray"
  spawnOnce "kitty"
  spawnOnce "chromium"
  spawnOnce "thunderbird"
  spawnOnce "conky-manager"
  spawnOnce "sh ~/.conky/conky-startup.sh"
  spawnOnce "/opt/deepinwine/apps/Deepin-WeChat/run.sh"

modm :: KeyMask
modm = mod4Mask

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  setRandomWallpaper
    [ "$HOME/Dropbox/Screen/girls_with_guns_anime_girl_butterfly_101109_1920x1080.jpg"
    , "$HOME/Dropbox/Screen/a1cdfcaa3fcc7b143197f71b363523beb4cdf236.jpg"
    ]
  xmonad
    $                docks
    $                desktopConfig
                       { workspaces      = myWorkplace
                       , manageHook = manageDocks <+> myManageHook <+> manageHook def
                       , layoutHook      = avoidStruts myLayout
                       , terminal        = myTerminal
                       , logHook         = dynamicLogWithPP xmobarPP
                                             { ppOutput =  hPutStrLn xmproc
                                             , ppTitle = xmobarColor "green" "" . shorten 60
                                             }
                       , handleEventHook = ewmhDesktopsEventHook
                       , startupHook     = myStartupHook
                       , modMask         = modm     -- Rebind Mod to the Windows key
                       }
    `additionalKeys` customerKeyMaps

customerKeyMaps =
  [
          -- Mute volume.
    ( (0, xF86XK_AudioMute)
    , void toggleMute
    )
        -- Decrease volume.
  , ( (0, xF86XK_AudioLowerVolume)
    , void $ lowerVolume 5
    )
        -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume) , void $ raiseVolume 5)
  , ((0, xF86XK_MonBrightnessUp)  , spawn "xbacklight -inc 10")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
  , ( (modm .|. shiftMask, xK_a)
    , spawn
      "sleep 0.2; scrot -s '/home/chenyu/screen/%F_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'"
    )
  , ((modm .|. shiftMask, xK_p)                , spawn "passmenu")
  , ((modm, xK_m)                              , manPrompt popupConfig)
  , ((modm .|. controlMask .|. shiftMask, xK_p), passGeneratePrompt popupConfig)
  , ((modm, xK_s)                              , sshPrompt popupConfig)
  , ( (modm .|. shiftMask, xK_z)
    , spawn "xscreensaver-command -lock; xset dpms force off"
    )
  , ( (modm .|. controlMask, xK_h)
    , launcherPrompt popupConfig $ defaultLauncherModes launcherConfig
    )
  , ( (0, xK_Print)
    , spawn
      "scrot '/home/chenyu/screen/%F_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'"
    )
        -- %F Equivalent to %Y-%m-%d (the ISO 8601 date format).
        -- %T The time in 24-hour notation (%H:%M:%S).
        -- $w image width
        -- $h image height
  ]

launcherConfig :: LauncherConfig
launcherConfig = LauncherConfig
  { pathToHoogle = "/home/chenyu/.local/bin/hoogle"
  , browser      = "/usr/bin/chromium"
  }

popupConfig :: XPConfig
popupConfig = def { font = "xft:Inconsolata Nerd Font Complete:antialias=true"
                  , position = Top
                  , height = 48
                  }

myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full
 where
     -- default tiling algorithm partitions the screen into two panes
  tiled   = Tall nmaster delta ratio
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2
  -- Percent of screen to increment by when resizing panes
  delta   = 2 / 100
