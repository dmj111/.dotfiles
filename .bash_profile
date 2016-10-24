# ~/.bash_profile: executed by bash for login shells.

# export CDPATH=".:..:../..:~"

export HISTIGNORE="&:ls:exit:h:l"

export SHELL=$(which bash)

export LESS="-Ri"
export PAGER="less"

PATH=/usr/local/bin:$PATH

# export PYTHONPATH=$HOME/lib/python

export EDITOR=vi

# export INFOPATH=$HOME/lib/emacs-site-lisp/org-mode/doc:$INFOPATH
# export INFOPATH=$HOME/lib/emacs-site-lisp/dvc/BUILD/texinfo:$INFOPATH
# export LANG=ASCII


[[ -f ~/.bashrc ]] && . ~/.bashrc

# Local Variables:
# mode: shell-script
# End:
