--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Monokai"
author="Wimer Hazenberg (http://www.monokai.nl)"
-- Colors
base00="#272822"
base01="#383830"
base02="#49483e"
base03="#75715e"
base04="#a59f85"
base05="#f8f8f2"
base06="#f5f4f1"
base07="#f9f8f5"
base08="#f92672"
base09="#fd971f"
base0A="#f4bf75"
base0B="#a6e22e"
base0C="#a1efe4"
base0D="#66d9ef"
base0E="#ae81ff"
base0F="#cc6633"

