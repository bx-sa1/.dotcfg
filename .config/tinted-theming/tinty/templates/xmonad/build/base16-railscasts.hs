--Place this file in your .xmonad/lib directory and import module Colors into .xmonad/xmonad.hs config
--The easy way is to create a soft link from this file to the file in .xmonad/lib using ln -s
--Then recompile and restart xmonad.

module Colors
    ( scheme, author
    , base00, base01, base02, base03, base04, base05, base06, base07
    , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F
    ) where

scheme="Railscasts"
author="Ryan Bates (http://railscasts.com)"
-- Colors
base00="#2b2b2b"
base01="#272935"
base02="#3a4055"
base03="#5a647e"
base04="#d4cfc9"
base05="#e6e1dc"
base06="#f4f1ed"
base07="#f9f7f3"
base08="#da4939"
base09="#cc7833"
base0A="#ffc66d"
base0B="#a5c261"
base0C="#519f50"
base0D="#6d9cbe"
base0E="#b6b3eb"
base0F="#bc9458"

