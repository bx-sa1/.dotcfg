--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Solarized Light"
author="Ethan Schoonover (modified by aramisgithub)"
-- Colors
base00="#fdf6e3"
base01="#eee8d5"
base02="#93a1a1"
base03="#839496"
base04="#657b83"
base05="#586e75"
base06="#073642"
base07="#002b36"
base08="#dc322f"
base09="#cb4b16"
base0A="#b58900"
base0B="#859900"
base0C="#2aa198"
base0D="#268bd2"
base0E="#6c71c4"
base0F="#d33682"

