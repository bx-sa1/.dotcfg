[[ -f "$HOME/.config/shellcfg/aliases.sh" ]] && source "$HOME/.config/shellcfg/aliases.sh"
[[ -f "$HOME/.config/shellcfg/functions.sh" ]] && source "$HOME/.config/shellcfg/functions.sh"
[[ -f "$HOME/.config/shellcfg/prompt.sh" ]] && source "$HOME/.config/shellcfg/prompt.sh"

(cat ~/.cache/wal/sequences &)
source ~/.cache/wal/colors.sh

PROMPT="%F{3}%n@%m %~ %{$(git_ps1_color)%}$(__git_ps1 '(%s)')"$'\n'"%F{6}%# %f"
