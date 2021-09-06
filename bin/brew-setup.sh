#!/usr/bin/env bash

set -x

# Shell
brew install git --without-completions
brew install tmux
brew install zsh


# C++
brew install clang-format
brew install cmake
brew install llvm
brew install ninja

# js
brew install node

# Emacs
brew cask install emacs
brew install ispell


# nerd fonts
brew tap homebrew/cask-fonts
brew install --cask font-hack-nerd-font
