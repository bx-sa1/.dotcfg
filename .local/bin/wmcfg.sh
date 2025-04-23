#!/bin/bash
if [[ "$#" -eq 0 ]]; then
    echo "No WM given as argument."
    exit
fi

case "$1" in
    xmonad) cfg="$HOME/.config/xmonad/xmonad.hs" ;;
    *)
        echo "Not a valid WM."
        exit
        ;;
esac

nvim "$cfg"
