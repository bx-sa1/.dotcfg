--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="standardized-light"
author="ali (https://github.com/ali-githb/base16-standardized-scheme)"
-- Colors
base00="#ffffff"
base01="#eeeeee"
base02="#cccccc"
base03="#767676"
base04="#767676"
base05="#444444"
base06="#333333"
base07="#222222"
base08="#d03e3e"
base09="#d7691d"
base0A="#ad8200"
base0B="#31861f"
base0C="#00998f"
base0D="#3173c5"
base0E="#9e57c2"
base0F="#895025"

