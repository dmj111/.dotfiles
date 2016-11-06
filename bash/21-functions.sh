#!/usr/bin/env bash

# functions here.

COL="-fg snow -bg black -cr snow -fn 8x13 -geometry 80x60"
function xterm { command xterm $COL +sb -cm -vb -title $HOSTNAME "$@" & }
function em { emacs $COL -title emacs "$@" & }
function dirs { builtin dirs -v ; }

alias mr="rxvt $COL -vb -title $HOSTNAME &"

# stty -ixon

