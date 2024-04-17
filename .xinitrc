#!/usr/bin/env bash

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

xsettingsd &
dunst &
polybar &
XDG_SESSION_TYPE=x11 plank &
xss-lock ~/.local/bin/xsecurelock-env.sh &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
nitrogen --restore &
dex -a &
exec bspwm 
