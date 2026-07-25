--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Harmonic16 Light"
author="Jannik Siebert (https://github.com/janniks)"
-- Colors
base00="#f7f9fb"
base01="#e5ebf1"
base02="#cbd6e2"
base03="#aabcce"
base04="#627e99"
base05="#405c79"
base06="#223b54"
base07="#0b1c2c"
base08="#bf8b56"
base09="#bfbf56"
base0A="#8bbf56"
base0B="#56bf8b"
base0C="#568bbf"
base0D="#8b56bf"
base0E="#bf568b"
base0F="#bf5656"

