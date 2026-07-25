--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="selenized-black"
author="Jan Warchol (https://github.com/jan-warchol/selenized) / adapted to base16 by ali"
-- Colors
base00="#181818"
base01="#252525"
base02="#3b3b3b"
base03="#777777"
base04="#777777"
base05="#b9b9b9"
base06="#dedede"
base07="#dedede"
base08="#ed4a46"
base09="#e67f43"
base0A="#dbb32d"
base0B="#70b433"
base0C="#3fc5b7"
base0D="#368aeb"
base0E="#a580e2"
base0F="#eb6eb7"

