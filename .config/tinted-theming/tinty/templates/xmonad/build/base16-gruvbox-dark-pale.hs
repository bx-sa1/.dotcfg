--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Gruvbox dark, pale"
author="Dawid Kurek (dawikur@gmail.com), morhetz (https://github.com/morhetz/gruvbox)"
-- Colors
base00="#262626"
base01="#3a3a3a"
base02="#4e4e4e"
base03="#8a8a8a"
base04="#949494"
base05="#dab997"
base06="#d5c4a1"
base07="#ebdbb2"
base08="#d75f5f"
base09="#ff8700"
base0A="#ffaf00"
base0B="#afaf00"
base0C="#85ad85"
base0D="#83adad"
base0E="#d485ad"
base0F="#d65d0e"

