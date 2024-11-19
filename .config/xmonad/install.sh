#!/bin/bash

cabal update
cabal install --package-env=$HOME/.config/xmonad --lib base xmonad xmonad-contrib directories filepath
cabal install --package-env=$HOME/.config/xmonad xmonad
