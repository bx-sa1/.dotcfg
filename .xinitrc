#!/usr/bin/env bash

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile
[ -f ~/.cache/wal/colors.Xresources ] && xrdb -merge -I$HOME ~/.cache/wal/colors.Xresources
[ -f ~/.Xresources ] && xrdb -merge -I$Home ~/.Xresources

export XSESSION_PID="$$"

session=${1:-bspwm}
case $session in
bspwm) exec bspwm ;;
openbox) exec openbox-session ;;
xmonad) exec xmonad ;;
herbst) exec herbstluftwm ;;
esac
