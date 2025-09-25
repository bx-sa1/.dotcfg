#!/usr/bin/env bash

if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

[ -f ~/.Xresources ] && xrdb -merge -I"$HOME ~/.Xresources"

export XSESSION_PID="$$"

xsetroot -cursor_name left_ptr &

function startwm() {
	xsettingsd &
	xscreensaver --no-splash &
	xss-lock --ignore-sleep -- xscreensaver-command -lock &
	/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

	"$HOME/.config/polybar/launch.sh" &
	dunst &
	setbg reset &
	clipcatd &
	dex -a -w &

	"$HOME/.config/sxhkd/launch.sh" "$HOME/.config/sxhkd/keysrc" &

	exec "$1"
}

session=${1:-bspwm}
case $session in
bspwm) startwm bspwm ;;
openbox) startwm openbox-session ;;
xmonad) startwm xmonad ;;
herbst) startwm herbstluftwm ;;
mate) exec mate-session ;;
xfce) exec startxfce4 ;;
lxqt) exec startlxqt ;;
kde)
	export DESKTOP_SESSION=plasma
	exec /usr/bin/startplasma-x11
	;;
steam)
	exec gamescope -e -- steam -tenfoot
	;;

esac
