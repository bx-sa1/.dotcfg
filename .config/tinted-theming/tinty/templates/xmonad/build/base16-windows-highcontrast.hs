--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Windows High Contrast"
author="Fergus Collins (https://github.com/ferguscollins)"
-- Colors
base00="#000000"
base01="#1c1c1c"
base02="#383838"
base03="#545454"
base04="#a2a2a2"
base05="#c0c0c0"
base06="#dedede"
base07="#fcfcfc"
base08="#fc5454"
base09="#808000"
base0A="#fcfc54"
base0B="#54fc54"
base0C="#54fcfc"
base0D="#5454fc"
base0E="#fc54fc"
base0F="#008000"

