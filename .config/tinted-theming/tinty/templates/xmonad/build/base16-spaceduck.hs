--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Spaceduck"
author="Guillermo Rodriguez (https://github.com/pineapplegiant), packaged by Gabriel Fontes (https://github.com/Misterio77)"
-- Colors
base00="#16172d"
base01="#1b1c36"
base02="#30365f"
base03="#686f9a"
base04="#818596"
base05="#ecf0c1"
base06="#c1c3cc"
base07="#ffffff"
base08="#e33400"
base09="#e39400"
base0A="#f2ce00"
base0B="#5ccc96"
base0C="#00a3cc"
base0D="#7a5ccc"
base0E="#b3a1e6"
base0F="#ce6f8f"

