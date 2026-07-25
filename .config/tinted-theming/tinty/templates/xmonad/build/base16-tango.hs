--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Tango"
author="@Schnouki, based on the Tango Desktop Project"
-- Colors
base00="#2e3436"
base01="#8ae234"
base02="#fce94f"
base03="#555753"
base04="#729fcf"
base05="#d3d7cf"
base06="#ad7fa8"
base07="#eeeeec"
base08="#cc0000"
base09="#ef2929"
base0A="#c4a000"
base0B="#4e9a06"
base0C="#06989a"
base0D="#3465a4"
base0E="#75507b"
base0F="#34e2e2"

