--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Ashes"
author="Jannik Siebert (https://github.com/janniks)"
-- Colors
base00="#1c2023"
base01="#393f45"
base02="#565e65"
base03="#747c84"
base04="#adb3ba"
base05="#c7ccd1"
base06="#dfe2e5"
base07="#f3f4f5"
base08="#c7ae95"
base09="#c7c795"
base0A="#aec795"
base0B="#95c7ae"
base0C="#95aec7"
base0D="#ae95c7"
base0E="#c795ae"
base0F="#c79595"

