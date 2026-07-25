--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Material"
author="Nate Peterson"
-- Colors
base00="#263238"
base01="#2e3c43"
base02="#314549"
base03="#546e7a"
base04="#b2ccd6"
base05="#eeffff"
base06="#eeffff"
base07="#ffffff"
base08="#f07178"
base09="#f78c6c"
base0A="#ffcb6b"
base0B="#c3e88d"
base0C="#89ddff"
base0D="#82aaff"
base0E="#c792ea"
base0F="#ff5370"

