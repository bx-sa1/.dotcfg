--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="tender"
author="Jacobo Tabernero (https://github/com/jacoborus/tender.vim)"
-- Colors
base00="#282828"
base01="#383838"
base02="#484848"
base03="#4c4c4c"
base04="#b8b8b8"
base05="#eeeeee"
base06="#e8e8e8"
base07="#feffff"
base08="#f43753"
base09="#dc9656"
base0A="#ffc24b"
base0B="#c9d05c"
base0C="#73cef4"
base0D="#b3deef"
base0E="#d3b987"
base0F="#a16946"

