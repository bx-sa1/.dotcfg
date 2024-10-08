#!/usr/bin/env bash

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xsettingsd &
xscreensaver --no-splash &
xss-lock -- xscreensaver-command --lock &
pcmanfm -d &
clipcatd &

session=${1:-bspwm}
case $session in
bspwm) exec bspwm ;;
openbox) exec openbox-session ;;
esac
