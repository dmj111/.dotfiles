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


# Local local
if [ -f ~/.local-dotfiles/.bash_profile ]; then
    builtin source ~/.local-dotfiles/.bash_profile
fi

# Load bashrc in login shells.
if [ -f ~/.bashrc ]; then
    builtin source ~/.bashrc
fi

# Local Variables:
# mode: shell-script
# End:
