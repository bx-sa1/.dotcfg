--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="PaperColor Light"
author="Jon Leopard (http://github.com/jonleopard), based on PaperColor Theme (https://github.com/NLKNguyen/papercolor-theme)"
-- Colors
base00="#eeeeee"
base01="#af0000"
base02="#008700"
base03="#5f8700"
base04="#0087af"
base05="#444444"
base06="#005f87"
base07="#878787"
base08="#bcbcbc"
base09="#d70000"
base0A="#d70087"
base0B="#8700af"
base0C="#d75f00"
base0D="#d75f00"
base0E="#005faf"
base0F="#005f87"

