#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
[[ -f "$HOME/.config/shellcfg/env.sh" ]] && source "$HOME/.config/shellcfg/env.sh"

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx $HOME/.xinitrc xfce -- -keeptty >~/.xorg.log 2>&1
fi
