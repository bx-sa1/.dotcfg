import XMonad
import XMonad.Config.Desktop

main :: IO ()
main = xmonad $ desktopConfig
  { modMask = mod4Mask
  , terminal = "alacritty"
  }
