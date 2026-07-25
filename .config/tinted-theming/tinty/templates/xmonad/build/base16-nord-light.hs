--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Nord Light"
author="threddast, based on fuxialexander&#39;s doom-nord-light-theme (Doom Emacs)"
-- Colors
base00="#e5e9f0"
base01="#c2d0e7"
base02="#b8c5db"
base03="#aebacf"
base04="#60728c"
base05="#2e3440"
base06="#3b4252"
base07="#29838d"
base08="#99324b"
base09="#ac4426"
base0A="#9a7500"
base0B="#4f894c"
base0C="#398eac"
base0D="#3b6ea8"
base0E="#97365b"
base0F="#5272af"

