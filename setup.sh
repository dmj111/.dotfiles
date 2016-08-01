#!/bin/zsh

# .gitconfig

files=(
    .bash_profile
    .bashrc
    .inputrc
    .jshintrc
)

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
print $DIR


for f in $files; do
    print $f
    rm -f ${HOME}/${f}
    src=${DIR}/${f}
    dst=${HOME}/${f}
    print $src $dst
    ln -sf ${src} ${dst}
    diff ${src} ${dst}
done


