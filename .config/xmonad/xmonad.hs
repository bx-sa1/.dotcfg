import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

main :: IO ()
main = do
  safeSpawn "mkfifo" ["/tmp/xmonad-polybar"]
  handle <- spawnPipe "stdbuf -i 0 tee /tmp/xmonad-polybar"
  xmonad $ ewmhFullscreen $ ewmh $ docks $ myConfig handle

myModMask = mod4Mask

myConfig handle = def
  { modMask = mod4Mask
  , normalBorderColor = "#928374"
  , focusedBorderColor =  "#d79921"
  , terminal = "alacritty"
  , workspaces = ["home","web","dev","music","games"]
  , startupHook = myStartupHook
  , manageHook = myManageHook
  , layoutHook =  myLayoutHook
  , logHook = myPP
  }
  `additionalKeysP` addKeysP
  `removeKeysP` delKeysP
  `additionalKeys` addKeys
  where 
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
    myLayoutHook = avoidStruts 
                  $ smartBorders 
                  $ spacingWithEdge 10 
                  $ tiled ||| Mirror tiled ||| Full
                    
    myStartupHook = do
      spawnOnce "nitrogen --restore"
      spawnOnce "dunst"
      spawnOnce "polybar xmonad"
      spawnOnce "picom -b"
      spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
      spawnOnce "dex -a -w"

    composedManageHook = composeAll
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
    myManageHook = placeHook (smart (0.5, 0.5))
                  <> manageDocks 
                  <+> manageHook def
                  <+> composedManageHook



    myPP = dynamicLogWithPP 
      $ def 
        { ppOutput = hPutStrLn handle 
        , ppOrder = \(_:l:_:_) -> [l]
        }

    addKeysP  = [ ("M-w", kill)

                , ("M-r", spawn "rofi -show combi -combi-modes drun,run,windows")
                , ("M-S-q", spawn "quit_rofi")
                , ("M-=", addWorkspacePrompt def)
                , ("M--", removeWorkspace)

                , ("M-\\ S-b", spawn "toggle-replay-buffer.sh")
                , ("M-\\ b", spawn "save-replay.sh")
                , ("<Print>", spawn "flameshot gui")

                , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 5%+")
                , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 5%-")
                , ("<XF86AudioMute>", spawn "amixer sset Master 1+ toggle")
                , ("<XF86AudioPlay>", spawn "playerctl play-pause")
                , ("<XF86AudioNext>", spawn "playerctl next")
                , ("<XF86AudioPrev>", spawn "playerctl previous")

                , ("M-v", spawn "clipcat-menu && sleep 1")
                , ("M-S-v", spawn "clipcat-menu && xdotool type \"$(xlip -out -selection clipboard\")")
                ]

    delKeysP  = [ "M-p"
                , "M-S-c"
                ]

    addKeys = zip (zip (repeat (myModMask)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
              ++ zip (zip (repeat (myModMask .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])

