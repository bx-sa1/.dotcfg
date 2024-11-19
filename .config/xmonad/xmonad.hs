import Control.Exception.Base
import Data.Map qualified as M
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import XMonad
import XMonad.Actions.CopyWindow (copy, copyToAll, killAllOtherCopies)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen)
import XMonad.Hooks.Place
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Spacing
import XMonad.StackSet (lookupWorkspace)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

loadColors :: IO [String]
loadColors = do
  home <- getHomeDirectory
  file <- try $ home `</>` readFile "~/.cache/wal/colors" :: IO (Either SomeException String)
  let colors = case file of
        Left e -> repeat "#222222"
        Right f -> take 8 $ lines f
  return colors

main :: IO ()
main = do
  safeSpawn "mkfifo" ["/tmp/xmonad-polybar"]
  handle <- spawnPipe "stdbuf -i 0 tee /tmp/xmonad-polybar"
  colors <- loadColors
  xmonad $ ewmhFullscreen . ewmh . docks $ myConfig handle colors

myModMask = mod4Mask

myConfig handle colors =
  def
    { modMask = mod4Mask,
      normalBorderColor = colors !! 1,
      focusedBorderColor = colors !! 5,
      terminal = "alacritty",
      workspaces = ["home", "web", "dev", "music", "games"],
      startupHook = myStartupHook,
      manageHook = myManageHook,
      layoutHook = myLayoutHook,
      logHook = myPP
    }
    `additionalKeysP` addKeysP
    `removeKeysP` delKeysP
    `additionalKeys` addKeys
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes
    myLayoutHook =
      avoidStruts $
        smartBorders $
          spacingWithEdge 10 $
            tiled ||| Mirror tiled ||| Full

    myStartupHook = do
      spawnOnce "wal -R"
      spawnOnce "dunst"
      spawnOnce "polybar xmonad"
      spawnOnce "picom -b"
      spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
      spawnOnce "dex -a -w"

    composedManageHook =
      composeAll
        [ className =? "firefox" --> doShift "web",
          className =? "steam" --> doShift "games",
          className =? "Lutris" --> doShift "games",
          className =? "Discord" --> doShift "games",
          className =? "REAPER" --> doShift "music",
          className =? "com.bitwig.BitwigStudio" --> doShift "music",
          className =? "Qtractor" --> doShift "music",
          className =? "Spotify" --> doShift "music",
          className =? "Pcmanfm" --> doFloat,
          className =? "Alacritty" --> doFloat,
          className =? "firefox" <&&> appName =? "Toolkit" --> doFloat
        ]
    myManageHook =
      placeHook (smart (0.5, 0.5))
        <> manageDocks
          <+> manageHook def
          <+> composedManageHook

    myPP =
      dynamicLogWithPP $
        def
          { ppOutput = hPutStrLn handle,
            ppOrder = \(_ : l : _ : _) -> [l]
          }

    addKeysP =
      [ ("M-w", kill),
        ("M-r", spawn "rofi -show combi -combi-modes drun,run,windows"),
        ("M-S-q", spawn "quit_rofi"),
        ("M-=", addWorkspacePrompt def),
        ("M--", removeWorkspace),
        ("M-\\ S-b", spawn "toggle-replay-buffer.sh"),
        ("M-\\ b", spawn "save-replay.sh"),
        ("<Print>", spawn "flameshot gui"),
        ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 5%+"),
        ("<XF86AudioLowerVolume>", spawn "amixer sset Master 5%-"),
        ("<XF86AudioMute>", spawn "amixer sset Master 1+ toggle"),
        ("<XF86AudioPlay>", spawn "playerctl play-pause"),
        ("<XF86AudioNext>", spawn "playerctl next"),
        ("<XF86AudioPrev>", spawn "playerctl previous"),
        ("M-v", spawn "clipcat-menu && sleep 1"),
        ("M-S-v", spawn "clipcat-menu && xdotool type \"$(xlip -out -selection clipboard\")"),
        ("M-s", windows copyToAll),
        ("M-S-s", killAllOtherCopies)
      ]

    delKeysP =
      [ "M-p",
        "M-S-c"
      ]

    addKeys =
      [ ((myModMask .|. m, kc), withNthWorkspace f w)
        | (kc, w) <- zip ([xK_1 .. xK_9] ++ [xK_0]) [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]
        ++ [ ((myModMask .|. m, kc), screenWorkspace sc >>= flip whenJust (windows . f))
             | (kc, sc) <- zip ([xK_1 .. xK_9] ++ [xK_0]) [0 ..],
               (f, m) <- [(W.view, mod1Mask), (W.shift, mod1Mask .|. shiftMask)]
           ]
