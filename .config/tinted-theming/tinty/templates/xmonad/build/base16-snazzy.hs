--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Snazzy"
author="Chawye Hsu (https://github.com/chawyehsu), based on Hyper Snazzy Theme (https://github.com/sindresorhus/hyper-snazzy)"
-- Colors
base00="#282a36"
base01="#34353e"
base02="#43454f"
base03="#78787e"
base04="#a5a5a9"
base05="#e2e4e5"
base06="#eff0eb"
base07="#f1f1f0"
base08="#ff5c57"
base09="#ff9f43"
base0A="#f3f99d"
base0B="#5af78e"
base0C="#9aedfe"
base0D="#57c7ff"
base0E="#ff6ac1"
base0F="#b2643c"

