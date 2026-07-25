--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Brewer"
author="Timothée Poisot (http://github.com/tpoisot)"
-- Colors
base00="#0c0d0e"
base01="#2e2f30"
base02="#515253"
base03="#737475"
base04="#959697"
base05="#b7b8b9"
base06="#dadbdc"
base07="#fcfdfe"
base08="#e31a1c"
base09="#e6550d"
base0A="#dca060"
base0B="#31a354"
base0C="#80b1d3"
base0D="#3182bd"
base0E="#756bb1"
base0F="#b15928"

