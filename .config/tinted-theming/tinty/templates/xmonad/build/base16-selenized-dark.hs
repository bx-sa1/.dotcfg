--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="selenized-dark"
author="Jan Warchol (https://github.com/jan-warchol/selenized) / adapted to base16 by ali"
-- Colors
base00="#103c48"
base01="#184956"
base02="#2d5b69"
base03="#72898f"
base04="#72898f"
base05="#adbcbc"
base06="#cad8d9"
base07="#cad8d9"
base08="#fa5750"
base09="#ed8649"
base0A="#dbb32d"
base0B="#75b938"
base0C="#41c7b9"
base0D="#4695f7"
base0E="#af88eb"
base0F="#f275be"

