--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Ocean"
author="Chris Kempson (http://chriskempson.com)"
-- Colors
base00="#2b303b"
base01="#343d46"
base02="#4f5b66"
base03="#65737e"
base04="#a7adba"
base05="#c0c5ce"
base06="#dfe1e8"
base07="#eff1f5"
base08="#bf616a"
base09="#d08770"
base0A="#ebcb8b"
base0B="#a3be8c"
base0C="#96b5b4"
base0D="#8fa1b3"
base0E="#b48ead"
base0F="#ab7967"

