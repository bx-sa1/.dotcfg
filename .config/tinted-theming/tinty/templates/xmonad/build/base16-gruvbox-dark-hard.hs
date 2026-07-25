--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Gruvbox dark, hard"
author="Dawid Kurek (dawikur@gmail.com), morhetz (https://github.com/morhetz/gruvbox)"
-- Colors
base00="#1d2021"
base01="#3c3836"
base02="#504945"
base03="#665c54"
base04="#bdae93"
base05="#d5c4a1"
base06="#ebdbb2"
base07="#fbf1c7"
base08="#fb4934"
base09="#fe8019"
base0A="#fabd2f"
base0B="#b8bb26"
base0C="#8ec07c"
base0D="#83a598"
base0E="#d3869b"
base0F="#d65d0e"

