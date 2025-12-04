#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ -f ~/.aliases ]] && source ~/.aliases
[[ -f ~/.functions ]] && source ~/.functions
[[ -f ~/.prompt ]] && source ~/.prompt
[[ -f ~/.colors ]] && source ~/.colors
[[ -f ~/.user ]] && source ~/.user

set -o vi

source /usr/share/bash-completion/completions/git
__git_complete dotcfg __git_main

PS1='\[\e[0;33m\]\u@\[\e[0;33m\]\h \[\e[0;33m\]\W\[$(git_ps1_color)\]$(__git_ps1 " (%s)")\n\[\e[0;36m\]\$ \[\e[0m\]'
