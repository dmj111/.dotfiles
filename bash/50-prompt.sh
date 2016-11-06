#!/usr/bin/env bash

prompt_func () {
    local screenw=""
    local B="\[\033[1m\]"
    local b="\[\033[0m\]"

    case $TERM in
        screen)
            screenw=" ($WINDOW) ";;
        xterm) ;;
        *) B=""; b="";;
    esac

    PS1="bash $B[\$(date +%H:%M)] \w [\u@\h"$screenw"]$b\n$B--\! \$ $b"
}
prompt_func


simple_prompt () {
    PS1="[\$(date +%H:%M)] \w [\u@\h"$screenw"]\n--\! \$ "
}
