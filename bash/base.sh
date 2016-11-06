

function readfile { [ -f $1 ] &&  . $1 ; }

#
# Git stuff
#

# Copy git-prompt.sh and git-completion.bash from the git
# contrib/completion directory here.

# [[ -f git-completion.bash ]] && . git-completion.bash
# [[ -f git-prompt.sh ]] && . git-prompt.sh


export PATH=$HOME/bin:$PATH

[[ -r ~/config/local/.bashrc ]] && source ~/config/local/.bashrc
# Local Variables:
# mode: shell-script
# End:


# PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
