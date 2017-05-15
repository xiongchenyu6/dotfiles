import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Config
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myManageHook = composeAll
  [ className =? "emacs"      --> doShift "1:dev"
  , className =? "emacsclient"  --> doShift "1:dev"
  , className =? "chromium-browser"  --> doShift "2:web"
  ,className =? "Chromium"       --> doShift "2:web"
  , className =? "Google-chrome"  --> doShift "2:web"
  , manageDocks
  ]

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        { workspaces = ["1:dev","2:web","3:game","4:comm","5:ham","6:tmp","7:dvi","8","9","0","-","="]
        ,manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
