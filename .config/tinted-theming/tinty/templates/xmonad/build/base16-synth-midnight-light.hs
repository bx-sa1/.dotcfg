--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Synth Midnight Terminal Light"
author="Michaël Ball (http://github.com/michael-ball/)"
-- Colors
base00="#dddfe0"
base01="#cfd1d2"
base02="#c1c3c4"
base03="#a3a5a6"
base04="#474849"
base05="#28292a"
base06="#1a1b1c"
base07="#050608"
base08="#b53b50"
base09="#ea770d"
base0A="#c9d364"
base0B="#06ea61"
base0C="#42fff9"
base0D="#03aeff"
base0E="#ea5ce2"
base0F="#cd6320"

