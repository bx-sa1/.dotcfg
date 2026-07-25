--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Dracula"
author="Jamy Golden (http://github.com/JamyGolden), based on Dracula Theme (http://github.com/dracula)"
-- Colors
base00="#282a36"
base01="#363447"
base02="#44475a"
base03="#6272a4"
base04="#9ea8c7"
base05="#f8f8f2"
base06="#f0f1f4"
base07="#ffffff"
base08="#ff5555"
base09="#ffb86c"
base0A="#f1fa8c"
base0B="#50fa7b"
base0C="#8be9fd"
base0D="#80bfff"
base0E="#ff79c6"
base0F="#bd93f9"

