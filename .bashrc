#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

(cat ~/.cache/wal/sequences &)
source ~/.cache/wal/colors-tty.sh

set -o vi

alias ls='ls --color=auto -la'
alias grep='grep --color=auto'
alias rip_sample='yt-dlp -x -P ~/Music/prod/SAMPLES/my-samples'
alias dotcfg="git --git-dir=$HOME/.dotcfg/ --work-tree=$HOME"
alias pls="expac -H M '%m\t%n' | sort -h"
alias sudo="sudo -E -s"
alias vim="nvim"
alias eb="emacs -batch -l ~/.config/emacs/init.el -eval"

eb '(org-batch-agenda "t")'

function gencxx {
    cat > Makefile << END 
TARGET_EXEC := final_program
BUILD_DIR := ./build
SRC_DIRS := ./src

SRCS := \$(shell find \$(SRC_DIRS) -name '*.cpp' -or -name '*.c' -or -name '*.s')
OBJS := \$(SRCS:%=\$(BUILD_DIR)/%.o)
DEPS := \$(OBJS:.o=.d)

INC_DIRS := \$(shell find \$(SRC_DIRS) -type d)
INC_FLAGS := \$(addprefix -I,\$(INC_DIRS))

CPPFLAGS := \$(INC_FLAGS) -MMD -MP

\$(BUILD_DIR)/\$(TARGET_EXEC): \$(OBJS)
	\$(CXX) \$(OBJS) -o \$@ \$(LDFLAGS)

\$(BUILD_DIR)/%.c.o: %.c
	mkdir -p \$(dir \$@)
	\$(CC) \$(CPPFLAGS) \$(CFLAGS) -c \$< -o \$@

\$(BUILD_DIR)/%.cpp.o: %.cpp
	mkdir -p \$(dir \$@)
	\$(CXX) \$(CPPFLAGS) \$(CXXFLAGS) -c \$< -o \$@


.PHONY: clean
clean:
	rm -r \$(BUILD_DIR)

-include \$(DEPS)
END
}

function reload-vim {
  vim -S "Session.vim"
}

function xmonadcfg {
  nvim "$HOME/.config/xmonad/xmonad.hs"
}

function notes {
    notes_dir="$HOME/Documents/notes"
    if [ "$#" -eq 0 ]; then
        emacs -nw "$notes_dir/QuickNotes.org"
    else
        case "$1" in
        ls) 
            ls "$notes_dir"
            ;;
        *) 
            emacs -nw "$notes_dir/$1.org"
            ;;
        esac
    fi
}

function mkdircd {
  mkdir -p $@ && cd $@
}

function music2vid {
  random="$(find ~/Pictures/type-beat-bgs -type f | shuf -n 1 -)"
  audio="$1"
  image="${2:-$random}"

  ffmpeg -r 1 -loop 1 -i "$image" -i "$audio" -acodec copy -r 1 -shortest -vf "scale=1920:1080:force_original_aspect_ratio=1,pad=1920:1080:(( (ow - iw)/2 )):(( (oh - ih)/2 ))" -sws_flags lanczos out.avi
}

function zephyr-mine {
  gamemoderun xmrig -o zeph-eu1.nanopool.org:10943 -u ZEPHYR2N48MYrdAPBa7ZZAS9Eqne54Wx1XDeFzrjQhHRNnqxte1drRof25vryHnq3MeH4QWPTDUfz9fnPo6qPcWj62cdQ6jWMnD5L --tls -k -a rx/0
}

function monero-mine {
  sudo xmrig -o pool.hashvault.pro:80 -u 431RPBKtLqvS9dYHQpE71uezVrX3eZixeFvmSm4dLfp1SQio6ZiaTckGzpNbeXGm6XheRRDWc6xY6Aiqyt2otiKQA4Rw4Fk -p baba --tls --tls-fingerprint 420c7850e09b7c0bdcf748a7da9eb3647daf8515718f36d9ccfdd6b9ff834b14 -k -a rx/0
}

function wal-set {
    wal-custom -i "$@" \
    && xmonad --recompile && xmonad --restart \
    && spicetify apply
}
function walp {
    source ~/.cache/wal/colors.sh
    case "$1" in
        set) 
            shift
            wp=$(feh -A 'echo %F;killall feh' ~/.local/share/wallpapers/)
            wal-set "$wp" "$@"
            ;;
        reset)
            shift
            wp="$wallpaper"
            wal-set "$wp" "$@"
            ;;
        *)
            echo "Not a valid command"
            ;;
    esac
  
}

source /usr/share/bash-completion/completions/git
__git_complete dotcfg __git_main

source /usr/share/git/completion/git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUPSTREAM=1
GIT_PS1_STATESEPARATOR="|"
git_ps1_color() {
    [[ $(git status -s 2> /dev/null) ]] && echo -e "\e[0;34m" || echo -e "\e[0;35m"
}
PS1='\[\e[0;31m\]\u@\[\e[0;32m\]\h \[\e[0;33m\]\W\[$(git_ps1_color)\]$(__git_ps1 " (%s)")\n\[\e[0;36m\]\$ \[\e[0m\]'

[ -f "/home/baba/.ghcup/env" ] && . "/home/baba/.ghcup/env" # ghcup-env
export PATH=$PATH:/home/baba/.spicetify
[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh
export CRYPTOGRAPHY_OPENSSL_NO_LEGACY=1
