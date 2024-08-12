#!/usr/bin/env bash

set -ex

to_dotfiles=(
    README.md
    bin/
    dmj-tools/
    jupyter_notebook_config.py
    setup.sh
    home/.gitexcludes
)

mkdir -p .dotfiles

for f in "${to_dotfiles[@]}"; do
    git mv -k $f .dotfiles/
done

to_home=(
    home/.abcde.conf
    home/.bash_profile
    home/.bashrc
    home/.emacs.d/
    home/.gitconfig
    home/.inputrc
    home/.jshintrc.example
    home/.sample-eslintrc.yml
    home/.screenrc
    home/.tmux.conf
    home/.vimrc
    home/.vscode/
    home/.zsh/
    home/.zshrc
)

for f in "${to_home[@]}"; do
    git mv -k $f .
done
