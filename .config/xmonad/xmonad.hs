import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP



main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ xmobarProp $ myConfig

myConfig = def
  { modMask = myModMask
  , terminal = "alacritty"
  , startupHook = myStartupHook
  , workspaces = myWorkspaces
  } `additionalKeysP` myKeyMap 

myWorkspaces = ["home","web","dev","music","games"]
myModMask = mod4Mask

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore"
  spawnOnce "dunst"
  spawnOnce "polybar"
  spawnOnce "picom -b"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  spawnOnce "dex -a -w"

myKeyMap = 
  [ ("M-p", spawn "rofi -show combi -combi-modes drun,run,windows")
  ]

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "firefox" --> doShift "web"
  , className =? "steam" --> doShift "games"
  , className =? "Lutris" --> doShift "games"
  , className =? "Discord" --> doShift "games"
  , className =? "REAPER" --> doShift "music"
  ]
