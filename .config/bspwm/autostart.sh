#!/bin/sh

run () {
    if ! pgrep -f "$1" 
    then
        "$@"&
    fi
}

run sxhkd -m -1
run dunst
run $HOME/.config/polybar/launch.sh

run pcmanfm -d
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
run picom -b
run xsettingsd
run /usr/lib/geoclue-2.0/demos/agent
run redshift
run /usr/lib/kdeconnectd

run $HOME/.local/bin/welcome
run thunderbird
