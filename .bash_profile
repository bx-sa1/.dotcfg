#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export EDITOR=nvim
export OBS_VKCAPTURE=1
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export SDL_IM_MODULE=fcitx

export QT_QPA_PLATFORMTHEME=qt5ct

export SYNCPKG_REPOS="https://aur.archlinux.org"
export PATH="$PATH:/home/baba/.local/bin:/home/baba/.config/emacs/bin:/home/baba/.cargo/bin"
export PATH="$PATH:$(go env GOBIN):$(go env GOPATH)/bin"
export QSYS_ROOTDIR="/home/baba/.local/bin/intelFPGA_lite/22.1std/quartus/sopc_builder/bin"

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]; then
	exec startx &>/dev/null
fi
