# ~/.bash_profile: executed by bash for login shells.

export HISTIGNORE="&:ls:exit:h:l"

# export SHELL=$(which bash)
export LESS="-Ri"
export PAGER="less"

PATH=/usr/local/bin:$PATH

export EDITOR=vim

# export INFOPATH=$HOME/lib/emacs-site-lisp/org-mode/doc:$INFOPATH
# export INFOPATH=$HOME/lib/emacs-site-lisp/dvc/BUILD/texinfo:$INFOPATH
# export LANG=ASCII


function load_if_exists() {
    if [ -r $1 ]; then
        echo "loading $1"
        builtin source $1
    fi
}

# Local settings
load_if_exists ~/.dotfiles/local/.bash_profile

# Load bashrc in login shells.
load_if_exists ~/.bashrc

if [ -f ~/.bashrc ]; then
    builtin source ~/.bashrc
fi
