# ~/.bash_profile: executed by bash for login shells.

# ~/.bash_profile: executed by bash for login shells.

export HISTIGNORE="&:ls:exit:h:l"

export SHELL=$(which bash)
export LESS="-Ri"
export PAGER="less"

PATH=/usr/local/bin:$PATH

export EDITOR=vi

# export INFOPATH=$HOME/lib/emacs-site-lisp/org-mode/doc:$INFOPATH
# export INFOPATH=$HOME/lib/emacs-site-lisp/dvc/BUILD/texinfo:$INFOPATH
# export LANG=ASCII


# Load bashrc in login shells.
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

# Local Variables:
# mode: shell-script
# End:
