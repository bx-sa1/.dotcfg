#!/bin/sh
wal -n "$@"
feh --bg-fill "$(<"${HOME}/.cache/wal/wal")"

#reload important programs
xmonad --recompile && xmonad --restart &
spicetify apply &
wait
