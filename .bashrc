#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias rip_sample='yt-dlp -x -P ~/Music/prod/SAMPLES/my-samples'
alias dotcfg="git --git-dir=$HOME/.dotcfg/ --work-tree=$HOME"

source /usr/share/bash-completion/completions/git
__git_complete dotcfg __git_main

PS1='[\u@\h \W]\$ '
