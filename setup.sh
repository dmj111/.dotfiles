#!/usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


# Consider:
# zsh/local/locals.zsh
#     fpath=(<brew_location>/share/zsh-completions $fpath)
#     path=(<brew_location>/bin $path)


pushd $DIR/home
for f in $(ls -A); do
    src=${DIR}/home/${f}
    dst=${HOME}/${f}
    if [ "${src}" -ef "${dst}" ]; then
        echo "   ${src} and ${dst}  are the same"
    elif [ -e "${dst}" ]; then
        echo "$dst exists and is different than $src, not modifying"
    elif [ -d "${src}" ]; then
        echo "adding symbolic link from ${src} to ${dst}"
        ln -s "${src}" "${dst}"
    else
        echo "adding hard link from ${src} to ${dst}"
        ln  "${src}" "${dst}"
    fi
done
