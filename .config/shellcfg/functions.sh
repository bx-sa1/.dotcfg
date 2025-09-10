function save_as_sample {
    sdir="$HOME/Music/prod/SAMPLES/my-samples/$1"
    echo "$sdir"
    mkdir -p "$sdir"
    cp -t "$sdir" "$2"
}

function reload-vim {
    vim -S "Session.vim"
}

function xmonadcfg {
    nvim "$HOME/.config/xmonad/xmonad.hs"
}

function mkdircd {
    mkdir -p "$@" && cd "$@" || return
}

function cdsc {
    cd "$HOME/Music/prod/SUPERCOLLIDER/" || return
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
