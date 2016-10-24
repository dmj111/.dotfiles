# Start up zsh if available, but not if NO_SWITCH is set.
PATH=/usr/local/bin:$PATH

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

# load aliases and functions

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

[ -f $DIR/aliases ] && source $DIR/aliases
[ -f $DIR/functions ] && source $DIR/functions


shopt -s cdable_vars
shopt -s cdspell

set bell-style none

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

PROMPT_COMMAND='history -a; history -n'
shopt -s histappend

# function xterm { command xterm $COL +sb -cm -vb -title $HOSTNAME "$@" & }
#function em { emacs $COL -title emacs "$@" & }
#function dirs { builtin dirs -v ; }
# COL="-fg snow -bg black -cr snow -fn 8x13 -geometry 80x60"
# alias mr="rxvt $COL -vb -title $HOSTNAME &"

#stty -ixon

function readfile { [ -f $1 ] &&  . $1 ; }

#
# Git stuff
#

# Copy git-prompt.sh and git-completion.bash from the git
# contrib/completion directory here.

# [[ -f git-completion.bash ]] && . git-completion.bash
# [[ -f git-prompt.sh ]] && . git-prompt.sh

. ~/git-completion.bash

if [ -f ~/git-prompt.sh ]; then
    . ~/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    export PS1='\w$(__git_ps1 " (%s)")\$ '
else
    echo "git-prompt.sh not found"
fi
    

export PATH=$HOME/bin:$PATH

[[ -r ~/config/local/.bashrc ]] && source ~/config/local/.bashrc
# Local Variables:
# mode: shell-script
# End:


# PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
