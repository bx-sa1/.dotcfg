#!/bin/bash
dunst &
xsettingsd &
xss-lock -n /usr/share/doc/xss-lock/dim-screen.sh -- ~/.local/bin/xsecurelock-env.sh &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
