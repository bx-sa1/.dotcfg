#!/bin/bash

cp ~/.cache/wal/Colors.hs ~/.config/xmonad/Colors.hs
xmonad --recompile && xmonad --restart
