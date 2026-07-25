--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Windows 95 Light"
author="Fergus Collins (https://github.com/ferguscollins)"
-- Colors
base00="#fcfcfc"
base01="#e0e0e0"
base02="#c4c4c4"
base03="#a8a8a8"
base04="#7e7e7e"
base05="#545454"
base06="#2a2a2a"
base07="#000000"
base08="#a80000"
base09="#fcfc54"
base0A="#a85400"
base0B="#00a800"
base0C="#00a8a8"
base0D="#0000a8"
base0E="#a800a8"
base0F="#54fc54"

