--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Windows NT"
author="Fergus Collins (https://github.com/ferguscollins)"
-- Colors
base00="#000000"
base01="#2a2a2a"
base02="#555555"
base03="#808080"
base04="#a1a1a1"
base05="#c0c0c0"
base06="#e0e0e0"
base07="#ffffff"
base08="#ff0000"
base09="#808000"
base0A="#ffff00"
base0B="#00ff00"
base0C="#00ffff"
base0D="#0000ff"
base0E="#ff00ff"
base0F="#008000"

