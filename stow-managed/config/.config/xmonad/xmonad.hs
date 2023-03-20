{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- {-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import Control.Monad
import Graphics.X11.ExtraTypes.XF86
import System.Directory
import XMonad
import XMonad.Actions.Launcher
import XMonad.Actions.Volume
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WallpaperSetter
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Ssh()
import XMonad.Util.Brightness (change)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce

myTerminal :: String
myTerminal = "alacritty"

myManageHook =
  composeAll
    [ className =? "Alacritty" --> doShift "term",
      className =? "URxvt" --> doShift "term",
      className =? "Kitty" --> doShift "term",
      className =? "kitty" --> doShift "term",
      className =? "Chromium" --> doShift "web",
      className =? "Emacs" --> doShift "edit",
      className =? "Brave-browser" --> doShift "web",
      className =? "Thunderbird" --> doShift "email",
      className =? "Wechat" --> doShift "chat",
      className =? "Conky-manager" --> doShift "chat",
      className =? "Conky" --> doShift "chat",
      className =? "conky" --> doShift "chat",
      className =? "Wechat" --> doShift "chat",
      className =? "wechat" --> doShift "chat",
      className =? "Slack" --> doShift "chat",
      className =? "stalonetray" --> doIgnore
    ]

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawn "xset r rate 180 60"
  spawnOnce "brave"
  -- spawnOnce "fcitx5"
  spawnOnce "dropbox start"
  -- spawnOnce "thunderbird"
  -- spawnOnce "compton --config ~/.xmonad/compton.conf"
  spawnOnce myTerminal
  -- spawnOnce "polybar"
  spawn "polybar-msg cmd restart"
  -- spawnOnce "polybar -r"
  
  -- spawnOnce "xmobar -x 1"

modm :: KeyMask
modm = mod4Mask

darkWallpaperDir :: IO FilePath
darkWallpaperDir = (++ "/Dropbox/WallPaper/dark") <$> getHomeDirectory

warmWallpaperDir :: IO FilePath
warmWallpaperDir = (++ "/Dropbox/WallPaper/warm") <$> getHomeDirectory

myWorkplace :: [String]
myWorkplace =
  ["term", "edit", "web", "chat", "email", "tmp"] ++ (show <$> [7 .. 9])
  
myNumRow = [xK_ampersand
           , xK_bracketleft
           , xK_braceleft
           , xK_braceright
           , xK_parenleft
           , xK_equal
           , xK_asterisk
           , xK_parenright
           , xK_plus]


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf =
  M.fromList $ [((m.|. modm, k), windows $ f i)
             | (i,k) <- zip (XMonad.workspaces conf) myNumRow
             , (f,m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  
mySB = statusBarProp "xmobar -x 0" (pure xmobarPP)

main :: IO ()
main = do
  xmonad . withSB mySB . ewmh
    . docks
    $ desktopConfig
      { workspaces = myWorkplace,
        manageHook = manageDocks <+> myManageHook <+> manageHook def,
        layoutHook = avoidStruts myLayout,
        terminal = myTerminal,
        logHook = setRandomWallpaper,
        startupHook = myStartupHook,
        keys = \c -> myKeys c `M.union` (keys def c),
        modMask = modm -- Rebind Mod to the Windows key
      }
      `additionalKeys` customerKeyMaps

--  setRandomWallpaper ["$HOME/Dropbox/WallPaper"] -- should install feh for the functionality
setRandomWallpaper :: X ()
setRandomWallpaper = do
  dwD <- io darkWallpaperDir
  wwD <- io warmWallpaperDir
  wallpaperSetter
    def
      { wallpapers =
          WallpaperList $
            zip
              myWorkplace
              ( cycle $
                  WallpaperDir
                    <$> [dwD, wwD]
              )
      }

customerKeyMaps =
  [ ( (0, xF86XK_AudioMute),
      void toggleMute
    ),
    -- Decrease volume.
    ( (0, xF86XK_AudioLowerVolume),
      void $ lowerVolume 5
    ),
    -- Increase volume.
    ((0, xF86XK_AudioRaiseVolume), void $ raiseVolume 5),
    ((0, xF86XK_MonBrightnessUp), void increase),
    ((0, xF86XK_MonBrightnessDown), void decrease),
    ( (modm .|. shiftMask, xK_a),
      spawn
        "sleep 0.2; scrot -s '/home/freeman.xiong/Pictures/%F--%H_%M_%S_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'"
    ),
    ((modm .|. shiftMask, xK_p), spawn "passmenu"),
    ((modm, xK_p), spawn "rofi -i -show drun -show-icons"),
    ((modm, xK_f), spawn "rofi -i -show filebrowser -show-icons"),
    ((modm, xK_s), spawn "rofi -i -show ssh -show-icons"),
    ((modm, xK_m), manPrompt popupConfig),

    ((modm .|. controlMask .|. shiftMask, xK_p), passGeneratePrompt popupConfig),
    ( (modm .|. shiftMask, xK_z),
      spawn "xscreensaver-command -lock; xset dpms force off"
    ),
    ( (modm .|. controlMask, xK_h),
      launcherPrompt popupConfig $ defaultLauncherModes launcherConfig
    ),
    ( (0, xK_Print),
      spawn
        "scrot '/home/freeman.xiong/screen/%F_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'"
    )
    -- %F Equivalent to %Y-%m-%d (the ISO 8601 date format).
    -- %T The time in 24-hour notation (%H:%M:%S). $wimagewidth $h image height
  ]

-- | Update brightness by +100
increase :: X ()
increase = liftIO $ change (+1024) *> (pure ())

-- | Update brightness by -100
decrease :: X ()
decrease = liftIO $ change (+ (-1024)) *> (pure ())

-- Mute volume.

launcherConfig :: LauncherConfig
launcherConfig =
  LauncherConfig
    { pathToHoogle = "/home/freeman.xiong/.local/bin/hoogle",
      browser = "/usr/bin/chromium"
    }

popupConfig :: XPConfig
popupConfig =
  def
    { font = "xft:Hack Nerd Font:size=15:antialias=true",
      position = Top,
      height = 48
    }

myLayout = avoidStruts $ Mirror tiled ||| tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 2 / 100
