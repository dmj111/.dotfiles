#!/usr/bin/env bash

# Try to run zshrc if NO_SWITCH is not set
function run_zsh {
    zsh_exe=$1
    [ -f ${zsh_exe} ] && exec ${zsh_exe} -l
}

if [ -z $NO_SWITCH ]; then
    case "$-" in
        *i*)
            run_zsh /bin/zsh
            run_zsh /usr/bin/zsh
            ;;
        *) ;;
    esac
fi
