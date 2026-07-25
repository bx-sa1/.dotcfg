--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="selenized-light"
author="Jan Warchol (https://github.com/jan-warchol/selenized) / adapted to base16 by ali"
-- Colors
base00="#fbf3db"
base01="#ece3cc"
base02="#d5cdb6"
base03="#909995"
base04="#909995"
base05="#53676d"
base06="#3a4d53"
base07="#3a4d53"
base08="#cc1729"
base09="#bc5819"
base0A="#a78300"
base0B="#428b00"
base0C="#00978a"
base0D="#006dce"
base0E="#825dc0"
base0F="#c44392"

