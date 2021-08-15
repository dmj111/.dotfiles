
# Setup for using git to control the init files

alias config="$(which git) --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
# checkout a branch:
# config checkout

# Create the local repo if needed
# git init --bare $HOME/code/.local-dotfiles/
# config config status.showUntrackedFiles no
alias local_config="$(which git) --git-dir=$HOME/code/.local-dotfiles/ --work-tree=$HOME"



