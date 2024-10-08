#!/bin/bash

bspc desktop -l next
layout=$(bspc wm -g | sed -e "s/^.*\(L[MT]\).*$/\1/")
if [[ $layout == "LM" ]]; then
  id=$(bspc query -N -n focused)
  tabbed=$(tabc.sh create $id)
  for windows in $(bspc query -N -d focused); do
    tabc.sh attach $tabbed $windows
  done
else
  tabbed=$(bspc query -N -n focused)
  while bspc query -N -n $tabbed >/dev/null; do
    tabc.sh detach $tabbed
  done
fi
