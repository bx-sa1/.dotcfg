source /usr/share/git/completion/git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUPSTREAM=1
GIT_PS1_STATESEPARATOR="|"
git_ps1_color() {
    [[ $(git status -s 2>/dev/null) ]] && echo -e "\e[0;34m" || echo -e "\e[0;35m"
}
