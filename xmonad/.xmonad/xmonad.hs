import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Config
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myManageHook = composeAll
  [ className =? "emacs"      --> doShift "edit"
  , className =? "emacsclient"  --> doShift "edit"
  , className =? "google-chrome-stable"  --> doShift "web"
  ]

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ docks $ defaultConfig
        { workspaces = ["term","edit","web","chat","email","6:tmp","7:dvi","8","9","0","-","="]
        , manageHook    = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , handleEventHook = ewmhDesktopsEventHook
        , startupHook   = ewmhDesktopsStartup <+> setWMName "LG3D"
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]

