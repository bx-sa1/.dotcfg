--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Bright"
author="Chris Kempson (http://chriskempson.com)"
-- Colors
base00="#000000"
base01="#303030"
base02="#505050"
base03="#b0b0b0"
base04="#d0d0d0"
base05="#e0e0e0"
base06="#f5f5f5"
base07="#ffffff"
base08="#fb0120"
base09="#fc6d24"
base0A="#fda331"
base0B="#a1c659"
base0C="#76c7b7"
base0D="#6fb3d2"
base0E="#d381c3"
base0F="#be643c"

