#!/usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


# Consider:
# zsh/local/locals.zsh
#     fpath=(<brew_location>/share/zsh-completions $fpath)
#     path=(<brew_location>/bin $path)

#!/usr/bin/env bash

safeln() {
    local src dst
    src=$1
    shift
    dst=$1
    shift
    echo "${src} -> ${dst}"

    # Full paths work, or relative paths to the dst if you are in the dst's directory
    if [ ! -e "${src}" ]; then
        echo "src does not exist"
    elif [ "${src}" -ef "${dst}" -a -h "${dst}" ]; then
        echo "they are the same"
    else
        if [ -e "${dst}" -o -h "${dst}" ]; then
            local f
            # Move original out of the way
            f=$(date "+${dst}.%Y%m%d_%H%M%S")
            echo "Copying ${dst} to ${f}"
            mv -n ${dst} ${f}
        fi
        ln -s ${src} ${dst}
    fi
}


pushd $DIR/home
for f in $(ls -A); do
    src=${DIR}/home/${f}
    dst=${HOME}/${f}
    safeln "${src}" "${dst}"
done

unset -f safeln
