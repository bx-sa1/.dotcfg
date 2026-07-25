--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Nova"
author="George Essig (https://github.com/gessig), Trevor D. Miller (https://trevordmiller.com)"
-- Colors
base00="#3c4c55"
base01="#556873"
base02="#6a7d89"
base03="#899ba6"
base04="#899ba6"
base05="#c5d4dd"
base06="#899ba6"
base07="#556873"
base08="#83afe5"
base09="#7fc1ca"
base0A="#a8ce93"
base0B="#7fc1ca"
base0C="#f2c38f"
base0D="#83afe5"
base0E="#9a93e1"
base0F="#f2c38f"

