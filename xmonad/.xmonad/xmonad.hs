{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- {-# LANGUAGE TupleSections #-}

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
import           XMonad.Hooks.WallpaperSetter
import           System.Directory


myTerminal :: String
myTerminal = "kitty"


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
  spawnOnce "compton --config ~/.xmonad/compton.conf"

modm :: KeyMask
modm = mod4Mask

darkWallpaperDir :: IO FilePath
darkWallpaperDir = (++ "/Dropbox/WallPaper/dark") <$> getHomeDirectory

warmWallpaperDir :: IO FilePath
warmWallpaperDir = (++ "/Dropbox/WallPaper/warm") <$> getHomeDirectory

myWorkplace :: [String]
myWorkplace =
  ["term", "edit", "web", "chat", "email", "tmp"] ++ (show <$> [7 .. 9])

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad
    $                docks
    $                desktopConfig
                       { workspaces      = myWorkplace
                       , manageHook = manageDocks <+> myManageHook <+> manageHook def
                       , layoutHook      = avoidStruts myLayout
                       , terminal        = myTerminal
                       , logHook         =
                         dynamicLogWithPP xmobarPP
                             { ppOutput = hPutStrLn xmproc
                             , ppTitle  = xmobarColor "green" "" . shorten 77
                             }
                           <+> do
                                 dwD <- io darkWallpaperDir
                                 wwD <- io warmWallpaperDir
                                 wallpaperSetter def
                                   { wallpapers = WallpaperList $ zip
                                                    myWorkplace
                                                    (cycle $ WallpaperDir <$> [dwD, wwD])
                                   }
                --(, WallpaperDir dwD) <$> myWorkplace
                       , handleEventHook = ewmhDesktopsEventHook
                       , startupHook     = myStartupHook
                       , modMask         = modm -- Rebind Mod to the Windows key
                       }
    `additionalKeys` customerKeyMaps

--  setRandomWallpaper ["$HOME/Dropbox/WallPaper"] -- should install feh for the functionality
customerKeyMaps =
  [ ( (0, xF86XK_AudioMute)
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
          -- Mute volume.

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
     -- default tiling algorithm partitions the screen into two panes
 where
  tiled   = Tall nmaster delta ratio
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2
  -- Percent of screen to increment by when resizing panes
  delta   = 2 / 100
