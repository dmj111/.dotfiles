#!/usr/bin/env bash


# Copy git-prompt.sh and git-completion.bash from the git
# contrib/completion directory here.


if [ -f $BASH_DIR/git-completion.bash ]; then
    echo "loading git-completion"
    . git-completion.bash
fi

if [ -f $BASH_DIR/git-prompt.sh ]; then
    . git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWUPSTREAM="auto,verbose,name"
    export PS1='\w$(__git_ps1 " (%s)")\$ '
fi
