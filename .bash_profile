#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export VISUAL=nvim
export EDITOR=nvim

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export SDL_IM_MODULE=fcitx

export OBS_VKCAPTURE=1
# export QT_QPA_PLATFORMTHEME=qt5ct
export GTK_MODULES="canberra-gtk-module:$GTK_MODULES"

export PATH=$PATH:/home/baba/.spicetify
export PATH="$PATH:/home/baba/.local/bin:/home/baba/.config/emacs/bin:/home/baba/.cargo/bin"
export PATH="$PATH:$(go env GOBIN):$(go env GOPATH)/bin"
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"

export GEM_HOME="$(gem env user_gemhome)"
export PATH="$PATH:$GEM_HOME/bin"

export npm_config_prefix="$HOME/.local/share/node"
export PATH="$PATH:$npm_config_prefix/bin"

export QSYS_ROOTDIR="/home/baba/.local/bin/intelFPGA_lite/22.1std/quartus/sopc_builder/bin"

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

case ":$PATH:" in
*:/home/baba/.juliaup/bin:*) ;;

*)
    export PATH=/home/baba/.juliaup/bin${PATH:+:${PATH}}
    ;;
esac

# <<< juliaup initialize <<<

export PATH="$PATH:/home/baba/.cache/scalacli/local-repo/bin/scala-cli"

# >>> coursier install directory >>>
export PATH="$PATH:/home/baba/.local/share/coursier/bin"
# <<< coursier install directory <<<

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx $HOME/.xinitrc xfce -- -keeptty >~/.xorg.log 2>&1
fi
