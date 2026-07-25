--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Darcula"
author="jetbrains"
-- Colors
base00="#2b2b2b"
base01="#323232"
base02="#323232"
base03="#606366"
base04="#a4a3a3"
base05="#a9b7c6"
base06="#ffc66d"
base07="#ffffff"
base08="#4eade5"
base09="#689757"
base0A="#bbb529"
base0B="#6a8759"
base0C="#629755"
base0D="#9876aa"
base0E="#cc7832"
base0F="#808080"

