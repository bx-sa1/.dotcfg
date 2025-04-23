#!/bin/bash

function set {
    wallpaper=$(feh -A 'echo %F;killall feh' ~/.local/share/wallpapers/);
    if [[ -n "$wallpaper" ]] then
        wal-custom.sh -i "$wallpaper" "$@"
    else 
        exit
    fi
}

function reset() {
    source ~/.cache/wal/colors.sh
    wal-custom.sh -i "$wallpaper" "$@"
}

case "$1" in
    set) 
        shift
        set "$@"
        ;;
    reset)
        shift
        reset "$@"
        ;;
    *)
        echo "Not a valid command"
        ;;
esac
