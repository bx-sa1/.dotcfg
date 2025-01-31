import Control.Exception.Base
import DBus.Notify qualified as N
import Data.Map qualified as M
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import XMonad
import XMonad.Actions.CopyWindow (copy, copyToAll, killAllOtherCopies)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Minimize
import XMonad.Actions.UpdateFocus
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.BoringWindows qualified as BW
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.Dishes
import XMonad.Layout.DragPane
import XMonad.Layout.FocusTracking
import XMonad.Layout.Fullscreen
import XMonad.Layout.IfMax
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Spacing
import XMonad.Layout.StackTile
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.StackSet (lookupWorkspace)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

sendNotif :: N.Client -> String -> IO ()
sendNotif client str = do
  let note = N.blankNote {N.summary = "XMonad", N.body = Just $ N.Text str}
  N.notify client note
  return ()

loadColors :: IO [String]
loadColors = do
  home <- getHomeDirectory
  let path = home </> ".cache/wal/colors"
  file <- try $ readFile path :: IO (Either SomeException String)
  let colors = case file of
        Left e -> repeat "#222222"
        Right f -> take 8 $ lines f
  return colors

myModMask = mod4Mask

myStartupHook = do
  spawnOnce "picom -b"
  spawnOnce "xsetroot -cursor_name left_ptr"

myLayoutHook =
  windowNavigation $
    focusTracking $
      smartBorders $
        avoidStruts $
          smartSpacingWithEdge 10 $
            minimize $
              BW.boringWindows $
                ifmax ||| Full
  where
    ifmax = IfMax 1 Full (tiled ||| Mirror tiled ||| twoPane ||| Mirror twoPane)
    twoPane = TwoPane delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myManageHook =
  placeHook (smart (0.5, 0.5))
    <> composedManageHook
    <> manageHook def
  where
    cShifts =
      [ ("firefox", "web"),
        ("steam", "games"),
        ("Lutris", "games"),
        ("discord", "games"),
        ("REAPER", "music"),
        ("com.bitwig.BitwigStudio", "music"),
        ("Qtractor", "music"),
        ("Spotify", "music")
      ]

    cFloats =
      [ "Localsend",
        "pavucontrol",
        "Anki",
        "Pcmanfm",
        "Alacritty",
        "Thunar",
        "zenity"
      ]

    isXfce = className ^? "Xfce4"

    composedManageHook =
      composeOne . concat $
        [ [title =? "Picture-in-Picture" -?> doFloat],
          [className =? c -?> doF (W.shift w) | (c, w) <- cShifts],
          [className =? c -?> doFloat | c <- cFloats],
          [checkDock -?> doRaise],
          [isNotification -?> doIgnore <+> doRaise],
          [isDialog -?> doFloat],
          [isXfce -?> doFloat],
          [willFloat -?> doFloat],
          [transience]
        ]

myPP =
  def
    { ppOrder = \(_ : l : _ : _) -> [l]
    }

myHandleEventHook = minimizeEventHook

addKeysP client =
  [ ("M-w", kill),
    ("M-=", addWorkspacePrompt def),
    ("M--", removeWorkspace),
    ("M-s", windows copyToAll),
    ("M-S-s", killAllOtherCopies),
    ("M-b", sendMessage ToggleStruts),
    ("M-C-S-h", sendMessage $ Move L),
    ("M-C-S-j", sendMessage $ Move D),
    ("M-C-S-k", sendMessage $ Move U),
    ("M-C-S-l", sendMessage $ Move R),
    ("M-<Space>", sendMessage NextLayout >> (dynamicLogString myPP >>= io . sendNotif client)),
    ("M-<Tab>", BW.focusDown),
    ("M-S-<Tab>", BW.focusUp),
    ("M-j", BW.focusDown),
    ("M-k", BW.focusUp),
    ("M-m", BW.focusMaster),
    ("M-S-j", BW.swapDown),
    ("M-S-k", BW.swapUp),
    ("M-C-m", withFocused minimizeWindow),
    ("M-C-S-m", withLastMinimized maximizeWindow)
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

main :: IO ()
main = do
  colors <- loadColors
  notifClient <- N.connectSession
  xmonad $
    ewmhFullscreen . ewmh . docks $
      def
        { modMask = myModMask,
          normalBorderColor = colors !! 1,
          focusedBorderColor = colors !! 5,
          terminal = "alacritty",
          workspaces = ["home", "web", "dev", "music", "games"],
          startupHook = myStartupHook,
          manageHook = myManageHook,
          layoutHook = myLayoutHook,
          handleEventHook = myHandleEventHook
        }
        `additionalKeysP` addKeysP notifClient
        `removeKeysP` delKeysP
        `additionalKeys` addKeys
