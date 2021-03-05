
case "$TERM" in
    "dumb")
        # For emacs tramp mode
        PS1='$ '
        return 0
        ;;
esac

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


if [ -f ~/.local-dotfiles/.bashrc-pre ]; then
    builtin source ~/.local-dotfiles/.bashrc-pre
fi

PATH=$HOME/bin:/usr/local/bin:$PATH

alias l=less
alias ll='ls -l'
alias ..='cd ..'

# Used to be necessary.
# stty -ixon

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


simple_prompt () {
    PS1="[\$(date +%H:%M)] \w [\u@\h"$screenw"]\n--\! \$ "
}

PROMPT_COMMAND='history -a; history -n'
shopt -s histappend



if [ -f ~/.local-dotfiles/.bashrc ]; then
    builtin source ~/.local-dotfiles/.bashrc
fi


prompt_func

# Copy git-prompt.sh and git-completion.bash from the git
# contrib/completion directory here.


if [ -f $BASH_DIR/git-completion.bash ]; then
    builtin source $BASH_DIR/git-completion.bash
fi

if [ -f $BASH_DIR/git-prompt.sh ]; then
    builtin source git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWUPSTREAM="auto,verbose,name"
    export PS1='\w$(__git_ps1 " (%s)")\$ '
fi

if [ -f ~/.local-dotfiles/.bashrc-post ]; then
    builtin source ~/.local-dotfiles/.bashrc-post
fi
