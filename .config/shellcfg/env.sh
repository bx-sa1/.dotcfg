[ -f "/home/baba/.ghcup/env" ] && . "/home/baba/.ghcup/env" # ghcup-env
[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh

export CRYPTOGRAPHY_OPENSSL_NO_LEGACY=1

export VISUAL=nvim
export EDITOR=nvim

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export SDL_IM_MODULE=fcitx

export GTK_MODULES="canberra-gtk-module:$GTK_MODULES"

export npm_config_prefix="$HOME/.local/share/node"
export QSYS_ROOTDIR="/home/baba/.local/bin/intelFPGA_lite/22.1std/quartus/sopc_builder/bin"
export ANDROID_HOME="$HOME/.android-sdk"

PATH="\
/home/baba/.spicetify:\
/home/baba/.local/bin:\
/home/baba/.config/emacs/bin:\
/home/baba/.cargo/bin:\
$(go env GOBIN):\
$(go env GOPATH)/bin:\
$HOME/.cabal/bin:\
$HOME/.ghcup/bin:\
$(gem env user_gemhome)/bin:\
$npm_config_prefix/bin:\
$ANDROID_HOME/cmdline-tools/latest/bin:\
$ANDROID_HOME/build-tools/36.1.0-rc1:\
$ANDROID_HOME/platform-tools:\
$PATH"
export PATH

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
