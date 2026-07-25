--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Catppuccin Latte"
author="https://github.com/catppuccin/catppuccin"
-- Colors
base00="#eff1f5"
base01="#e6e9ef"
base02="#ccd0da"
base03="#bcc0cc"
base04="#acb0be"
base05="#4c4f69"
base06="#dc8a78"
base07="#7287fd"
base08="#d20f39"
base09="#fe640b"
base0A="#df8e1d"
base0B="#40a02b"
base0C="#179299"
base0D="#1e66f5"
base0E="#8839ef"
base0F="#dd7878"

