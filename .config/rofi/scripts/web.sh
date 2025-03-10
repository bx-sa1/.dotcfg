#!/bin/bash
if [ "$#" -ne 0 ]; then 
    q="$*"
    echo "$q" >> /tmp/rofi-web-history
    cmd=$(echo "$q" | sed -n "s/^!.*$/xdg-open \"https:\/\/duckduckgo.com\/?t=h_\&q=&\"/p") 
    eval "$cmd"
else
    cat "/tmp/rofi-web-history"
fi
