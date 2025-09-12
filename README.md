# dotfile configuration

## Installation
```bash
$ git clone --bare https://github.com/bx-sa1/.dotcfg $HOME/.dotfiles
$ alias dotfiles='/usr/bin/git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME"'
$ dotfiles checkout
$ dotfiles config --local status.showUntrackedFiles no
```

## Dependencies
```
git
pywal
python
xfce
xmonad
```

Uses an edited pywal script `setbg` to change the colorscheme of applications based on the wallpaper, with subjectively better contrast in terminals.
