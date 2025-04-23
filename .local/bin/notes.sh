#!/bin/bash
notes_dir="$HOME/Documents/notes"

if [ "$#" -eq 0 ]; then
    emacs -nw "$notes_dir/QuickNotes.org"
    exit
fi

case "$1" in
ls) 
    ls "$notes_dir"
    ;;
*) 
    emacs -nw "$notes_dir/$1.org"
    ;;
esac
