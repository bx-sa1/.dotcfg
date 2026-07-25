--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Flat"
author="Chris Kempson (http://chriskempson.com)"
-- Colors
base00="#2c3e50"
base01="#34495e"
base02="#7f8c8d"
base03="#95a5a6"
base04="#bdc3c7"
base05="#e0e0e0"
base06="#f5f5f5"
base07="#ecf0f1"
base08="#e74c3c"
base09="#e67e22"
base0A="#f1c40f"
base0B="#2ecc71"
base0C="#1abc9c"
base0D="#3498db"
base0E="#9b59b6"
base0F="#be643c"

