#!/usr/bin/env bash
set -ex

cd $HOME/.dotfiles.git
git config --unset core.worktree
git config core.bare true
rm $HOME/.git
