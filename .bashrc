#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

todo.sh ls

alias ls='ls --color=auto -la'
alias grep='grep --color=auto'
alias rip_sample='yt-dlp -x -P ~/Music/prod/SAMPLES/my-samples'
alias dotcfg="git --git-dir=$HOME/.dotcfg/ --work-tree=$HOME"
alias pls="expac -H M '%m\t%n' | sort -h"
alias workout-log="nvim ~/Documents/journals/workout-log.csv"
alias t="todo.sh -t"
alias notes="nvim ~/Documents/notes/QuickNotes.md"
alias sudo="sudo -E -s"

function mkdircd {
  mkdir -p $@ && cd $@
}

function music2vid {
  image=$(find ~/Pictures/type-beat-bgs -type f | shuf -n 1 -)
  audio=$1

  ffmpeg -r 1 -loop 1 -i $image -i $audio -acodec copy -r 1 -shortest -vf "scale=1920:1080:force_original_aspect_ratio=1,pad=1920:1080:(( (ow - iw)/2 )):(( (oh - ih)/2 ))" -sws_flags lanczos out.avi
}

function scrrec-start {
  gpu-screen-recorder -w screen -f 60 -a "$(pactl get-default-sink).monitor" -a "$(pactl get-default-source)" -c mp4 -o "$HOME/Videos/Recordings/"
}

function scrrec-stop {
  killall -SIGINT gpu-screen-recorder
}

source /usr/share/bash-completion/completions/git
__git_complete dotcfg __git_main

PS1='[\u@\h \W]\$ '

[ -f "/home/baba/.ghcup/env" ] && . "/home/baba/.ghcup/env" # ghcup-env
export PATH=$PATH:/home/baba/.spicetify
