#!/bin/bash

set -e
cabal update
cabal install --package-env=$HOME/.config/xmonad --lib base xmonad xmonad-contrib  -j
cabal install --package-env=$HOME/.config/xmonad xmonad --overwrite-policy=always -j
