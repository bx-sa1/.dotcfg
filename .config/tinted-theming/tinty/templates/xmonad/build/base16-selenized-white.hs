--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="selenized-white"
author="Jan Warchol (https://github.com/jan-warchol/selenized) / adapted to base16 by ali"
-- Colors
base00="#ffffff"
base01="#ebebeb"
base02="#cdcdcd"
base03="#878787"
base04="#878787"
base05="#474747"
base06="#282828"
base07="#282828"
base08="#bf0000"
base09="#ba3700"
base0A="#af8500"
base0B="#008400"
base0C="#009a8a"
base0D="#0054cf"
base0E="#6b40c3"
base0F="#dd0f9d"

