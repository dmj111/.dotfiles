#!/usr/bin/env bash

# _git and git-completion-bash are copied from https://github.com/git/git/tree/master/contrib/completion
#
# https://medium.com/@oliverspryn/adding-git-completion-to-zsh-60f3b0e7ffbc

# Download the scripts
curl -o git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl -o _git https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.zsh
rm -f ~/.zcompdump
