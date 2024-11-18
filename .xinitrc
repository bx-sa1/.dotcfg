#!/usr/bin/env bash

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

export XSESSION_PID="$$"

session=${1:-bspwm}
case $session in
bspwm) exec bspwm ;;
openbox) exec openbox-session ;;
xmonad) exec xmonad ;;
herbst) exec herbstluftwm ;;
esac
