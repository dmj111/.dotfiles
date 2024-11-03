# START LOCAL CUSTOMIZATION
# END LOCAL CUSTOMIZATION

case "$TERM" in
    "dumb")
        # For emacs tramp mode
        PS1='$ '
        return 0
        ;;
esac

function run_zsh {
    zsh_exe=$1
    [ -f ${zsh_exe} ] && exec ${zsh_exe} -l
}

# This is for systems that default to bash but I want to use zsh.
# Sometimes I need bash for a specific task on those systems, so
# `NO_SWITCH=1 bash` is supposed to skip starting zsh
if [ -z $NO_SWITCH ]; then
    case "$-" in
        *i*)
            run_zsh /bin/zsh
            run_zsh /usr/bin/zsh
            ;;
        *) ;;
    esac
fi


function load_if_exists() {
    if [ -r $1 ]; then
        echo "loading $1"
        builtin source $1
    fi
}


# START LOCAL CUSTOMIZATION
# END LOCAL CUSTOMIZATION

export HISTSIZE=10000
export HISTFILESIZE=100000
export HISTIGNORE="&:ls:exit:h:l"

export SHELL=$(which bash)
export LESS="-Ri"
export PAGER="less"

PATH=/usr/local/bin:$PATH

export EDITOR=vim

# export INFOPATH=$HOME/lib/emacs-site-lisp/org-mode/doc:$INFOPATH
# export INFOPATH=$HOME/lib/emacs-site-lisp/dvc/BUILD/texinfo:$INFOPATH
# export LANG=ASCII

# START LOCAL CUSTOMIZATION
# END LOCAL CUSTOMIZATION

function restart {
    NO_SWITCH=1 exec bash -l
}


PATH=$HOME/bin:/usr/local/bin:$PATH

alias l=less
alias ..='cd ..'
alias ll='ls -lFh'
alias la='ls -lAFh'
alias lsl='ls -l'
alias lrt='ls -lAFhrt'
alias ldot='ls -ld .*'
alias lsort='ls -lrSAh'

alias grep='grep --color'
alias t='tail -f'

alias gg='git st'
alias gds='git diff --staged'
alias gd='git diff'

# Used to be necessary.
# stty -ixon

shopt -s cdable_vars
shopt -s cdspell

set bell-style none


bind '"\\ep": history-search-backward'

export LESS='-XFRi'


shopt -s histappend
HISTCONTROL=ignoredups:ignorespace
HISTIGNORE="ls:exit:h:l:ll:ls -lrt"
# Use history -r and history -w to manually read/write history


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


simple_prompt () {
    PS1="[\$(date +%H:%M)] \w [\u@\h"$screenw"]\n--\! \$ "
}

PROMPT_COMMAND='history -a'
shopt -s histappend



prompt_func

# Copy git-prompt.sh and git-completion.bash from the git
# contrib/completion directory here.


load_if_exists $BASH_DIR/git-completion.bash

if [ -f $BASH_DIR/git-prompt.sh ]; then
    builtin source $BASH_DIR/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWUPSTREAM="auto,verbose,name"
    export PS1='\w$(__git_ps1 " (%s)")\$ '
fi


# START LOCAL CUSTOMIZATION
# END LOCAL CUSTOMIZATION
load_if_exists $HOME/.dotfiles/local.bashrc


unset -f load_if_exists
