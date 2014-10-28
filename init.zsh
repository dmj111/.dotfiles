
# Use single quotes in the shell for words that have anything besides
# [A-Za-z0-9._] in them.

# To see the environment, use printenv.

# Set CDPATH for frequently used base directories.
# CDPATH=$HOME

# TODO: On linux, try: ls --color=auto

# Get emacs keybindings

[ -f ${ZSH}/local/local-pre.zsh ] && \
    source ${ZSH}/local/local-pre.zsh || print "no file"

setopt emacs


setopt no_beep


# TODO: Not sure if this is needed any more?
# bindkey -me

# setopt ignore_eof # don't let C-d kill the shell

# Set WORDCHARS so the shell will treat only chars/numbers as words
WORDCHARS=

# Search history, matching the line so far
bindkey "\ep" history-beginning-search-backward
bindkey "\en" history-beginning-search-forward

# Put current line on the command stack.
bindkey "\eq" push-line-or-edit

# make run-help nicer.  M-h brings up help for what you are doing
unalias run-help
autoload -U run-help



#### History
HISTSIZE=1000
SAVEHIST=10000
HISTFILE=~/.zsh_history





#### Aliases

alias l=less

alias -g L="| less"
alias -g LL="2>&1 | less"


#### Completion

autoload -U compinit
compinit -i -D



# Globbing
setopt extended_glob


# Change to a directory, if it is named on the command line.
setopt auto_cd

# Pressing space after a history command will expand it.
bindkey ' ' magic-space

# Give user chance to check that the history command was right.
setopt hist_verify

# Append from multiple shells.
setopt inc_append_history

# Keep time in history
setopt extended_history

# Ignore dups
setopt hist_ignore_dups

# Ignore commands starting with space
setopt hist_ignore_space

# Don't bother keeping function defs in history.
setopt hist_no_functions

## Autoloaded files, start with _, and are located on fpath.

[ -f ${ZSH}/local/local.zsh ] && \
    source ${ZSH}/local/local.zsh || print "no file"


# Useful information

# - C-u delete line
# - zsh is region aware (C-space, C-x C-x, M-x kill-region)
# - bindkey -L to see the listings
# - zle -la to see the zle commands
# - bindkey "\C-y" -- to see C-y's function
# - can bind strings to a key
# - create a key map by bindkey -N my-keymap parent-keymap
#     - emacs is a keymap
# - bindkey -A my-map main # alias my map to main
# - "\e-return" multi line input
# - zsh -f # no init



