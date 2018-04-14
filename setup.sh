#!/usr/bin/env bash


files=(
    .bash_profile
    .bashrc
    .gitconfig
    .inputrc
    .jshintrc
    .tmux.conf
    .zshrc
    .emacs.d
      )

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo $DIR


echo <<EOF
Consider:
zsh/local/locals.zsh
    fpath=(<brew_location>/share/zsh-completions $fpath)
    path=(<brew_location>/bin $path)

EOF

for f in "${files[@]}"; do
    echo $f
    # rm -f ${HOME}/${f}
    src=${DIR}/${f}
    dst=${HOME}/${f}
    echo $src $dst
    ln -sf ${src} ${dst}
    diff ${src} ${dst}
done
