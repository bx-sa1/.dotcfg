if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx "$HOME/.xinitrc" xfce -- -keeptty >"$HOME/.xorg.log" 2>&1
fi
