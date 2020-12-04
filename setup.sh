#!/usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"



# Consider:
# zsh/local/locals.zsh
#     fpath=(<brew_location>/share/zsh-completions $fpath)
#     path=(<brew_location>/bin $path)


pushd $DIR/home
for f in $(ls -A); do
    echo $f
    rm -f ${HOME}/${f}
    src=${DIR}/home/${f}
    dst=${HOME}/${f}
    ln -sf ${src} ${dst}
    echo $src $dst
    diff ${src} ${dst}
done

# for f in "${files[@]}"; do
#     echo $f
#     # rm -f ${HOME}/${f}
#     src=${DIR}/${f}
#     dst=${HOME}/${f}
#     echo $src $dst
#     ln -sf ${src} ${dst}
#     diff ${src} ${dst}
# done
