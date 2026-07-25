--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Tokyodark"
author="Jamy Golden (https://github.com/JamyGolden), Based on Tokyodark.nvim (https://github.com/tiagovla/tokyodark.nvim)"
-- Colors
base00="#11121d"
base01="#212234"
base02="#212234"
base03="#353945"
base04="#4a5057"
base05="#a0a8cd"
base06="#abb2bf"
base07="#bcc2dc"
base08="#ee6d85"
base09="#f6955b"
base0A="#d7a65f"
base0B="#95c561"
base0C="#9fbbf3"
base0D="#7199ee"
base0E="#a485dd"
base0F="#773440"

