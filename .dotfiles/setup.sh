#!/usr/bin/env bash

# Create links in $HOME from files/folders in the local home/
# directory. This script tries to avoid overwriting existing files if
# they have different content, but it isn't fool proof.
#
# The end result is the config files in $HOME are symlinked to the
# files in this folder.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

safeln() {
    local src dst
    src=$1
    shift
    dst=$1
    shift
    echo "${src} -> ${dst}"

    # Full paths work, or relative paths to the dst if you are in the dst's directory
    if [ ! -e "${src}" ]; then
        echo "${src} does not exist"
        return
    elif [ "${src}" -ef "${dst}" -a -h "${dst}" ]; then
        echo "already linked"
        return
    elif [ -e "${dst}" ]; then
        diff ${src} ${dst}
        if [ $? -ne 0 ]; then
            echo "*** files are different, not linking"
            return
        else
            echo "files are equal, linking..."
        fi
    else
        echo "${dst} does not exist, linking..."
    fi
    ln -sf ${src} ${dst}
}

cd $DIR/home
for f in $(ls -A); do
    src=${DIR}/home/${f}
    dst=${HOME}/${f}
    safeln "${src}" "${dst}"
done
