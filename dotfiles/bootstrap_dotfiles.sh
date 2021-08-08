# source this file to get the alias

alias config="$(which git) --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
config config status.showUntrackedFiles no
config checkout

