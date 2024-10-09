import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen

main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $  myConfig

myConfig = def
  { modMask = myModMask
  , terminal = "alacritty"
  , startupHook = myStartupHook
  , workspaces = myWorkspaces
  , manageHook = myManageHook <+> manageHook def
  , layoutHook = smartBorders $ spacingWithEdge 10 $ myLayoutHook
  } 
  `additionalKeysP`
  [ ("M-r", spawn "rofi -show combi -combi-modes drun,run,windows")
  , ("M-S-q", spawn "quit_rofi")
  ]
  `remapKeysP` 
  [ ("M-w", "M-S-c") 
  ]
  `removeKeysP`
  [ "M-p"
  ]
  

myWorkspaces = ["home","web","dev","music","games"]
myModMask = mod4Mask

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore"
  spawnOnce "dunst"
  spawnOnce "polybar xmonad"
  spawnOnce "picom -b"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  spawnOnce "dex -a -w"

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "firefox" --> doShift "web"
  , className =? "steam" --> doShift "games"
  , className =? "Lutris" --> doShift "games"
  , className =? "Discord" --> doShift "games"
  , className =? "REAPER" --> doShift "music"
  , className =? "com.bitwig.BitwigStudio" --> doShift "music"
  , className =? "Qtractor" --> doShift "music"
  , className =? "Spotify" --> doShift "music"
  , className =? "Pcmanfm" --> doFloat
  , className =? "Alacritty" --> doFloat
  , className =? "firefox" <&&> appName =? "Toolkit" --> doFloat <+> doIgnore
  ]

myLayoutHook = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

