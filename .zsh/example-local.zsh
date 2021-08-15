
# Setup for using git to control the init files

alias config="$(which git) --git-dir=$HOME/code/.dotfiles/ --work-tree=$HOME"
# checkout a branch:
# config checkout
# alternatively, set GIT_DIR and GIT_WORK_TREE env variable in a shell to
# work in the config directory

# Create the local repo if needed
# git init --bare $HOME/code/.local-dotfiles/
# config config status.showUntrackedFiles no
alias local_config="$(which git) --git-dir=$HOME/code/.local-dotfiles/ --work-tree=$HOME"



