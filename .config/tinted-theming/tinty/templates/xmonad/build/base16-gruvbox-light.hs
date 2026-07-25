--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Gruvbox Light"
author="Tinted Theming (https://github.com/tinted-theming), morhetz (https://github.com/morhetz/gruvbox)"
-- Colors
base00="#fbf1c7"
base01="#ebdbb2"
base02="#d5c4a1"
base03="#bdae93"
base04="#7c6f64"
base05="#3c3836"
base06="#282828"
base07="#1d2021"
base08="#cc241d"
base09="#d65d0e"
base0A="#d79921"
base0B="#98971a"
base0C="#689d6a"
base0D="#458588"
base0E="#b16286"
base0F="#9d0006"

