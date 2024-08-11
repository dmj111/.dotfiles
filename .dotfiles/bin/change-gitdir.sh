#!/usr/bin/env bash
set -ex
cd $HOME
echo "gitdir: $HOME/.dotfiles.git" > $HOME/.git
cd ~/.dotfiles.git
git config --unset core.bare
git config core.worktree ../
