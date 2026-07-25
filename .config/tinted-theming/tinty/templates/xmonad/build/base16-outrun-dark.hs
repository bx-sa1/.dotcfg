--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Outrun Dark"
author="Hugo Delahousse (http://github.com/hugodelahousse/)"
-- Colors
base00="#00002a"
base01="#20204a"
base02="#30305a"
base03="#50507a"
base04="#b0b0da"
base05="#d0d0fa"
base06="#e0e0ff"
base07="#f5f5ff"
base08="#ff4242"
base09="#fc8d28"
base0A="#f3e877"
base0B="#59f176"
base0C="#0ef0f0"
base0D="#66b0ff"
base0E="#f10596"
base0F="#f003ef"

