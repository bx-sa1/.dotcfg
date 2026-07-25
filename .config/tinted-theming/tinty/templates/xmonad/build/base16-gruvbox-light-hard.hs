--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Gruvbox light, hard"
author="Dawid Kurek (dawikur@gmail.com), morhetz (https://github.com/morhetz/gruvbox)"
-- Colors
base00="#f9f5d7"
base01="#ebdbb2"
base02="#d5c4a1"
base03="#bdae93"
base04="#665c54"
base05="#504945"
base06="#3c3836"
base07="#282828"
base08="#9d0006"
base09="#af3a03"
base0A="#b57614"
base0B="#79740e"
base0C="#427b58"
base0D="#076678"
base0E="#8f3f71"
base0F="#d65d0e"

