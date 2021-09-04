# Uncomment for profiling (and go to end of file)
# zmodload zsh/zprof

# echo "in ${0:a}"

# Add to this in local.zsh if desired.
post_init_hook=()


load_if_exists() {
    [[ -f $1 ]] && builtin source $1
}

load_if_exists $HOME/.dotfiles/local/pre.zsh

# For example:
#
# function local_post () {
#     fpath=($fpath /usr/local/share/zsh-completions)
# }
# post_init_hook+=local_post

#### Notes
# TODO:
# - hash -d to hash directory shortcuts

# Use single quotes in the shell for words that have anything besides
# [A-Za-z0-9._] in them.

# To see the environment, use printenv.

# Set CDPATH for frequently used base directories.
# CDPATH=$HOME

# TODO: On linux, try: ls --color=auto

### Useful information

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


#### Shell

# Restart the shell
function restart { exec $SHELL $SHELL_ARGS "$@" ; }

function runbash() {
    NO_SWITCH=1 command bash "$@"
}

# restart in bash

#### Environment


export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color

# [[ -r $HOME/Go ]] && export GOPATH=$HOME/Go

#### History
HISTSIZE=10000
SAVEHIST=100000
HISTFILE=$HOME/.zsh_history

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


#### UI

### options

# Use emacs keybindings
# bindkey -e
setopt emacs

setopt no_beep

setopt no_flow_control # C-s

# Globbing
setopt extended_glob

# Change to a directory, if it is named on the command line.
setopt auto_cd


# Set WORDCHARS so the shell will treat only chars/numbers as words
WORDCHARS=

## Keymaps
# Search history, matching the line so far
bindkey "\ep" history-beginning-search-backward
bindkey "\en" history-beginning-search-forward

# Put current line on the command stack.
bindkey "\eq" push-line-or-edit


# make run-help nicer.  M-h brings up help for what you are doing
# unalias run-help
autoload -U run-help
autoload -U run-help-git


# setopt ignore_eof # don't let C-d kill the shell

export LESS='-XFRi'

#### Aliases
alias l=less
alias ll='ls -l'
alias la='ls -la'
alias ..='cd ..'

alias -g L="| less"
alias -g LL="2>&1 | less"

alias en='emacsclient -n'
alias ec='emacsclient'

alias gl='git log --oneline --graph'
alias awake='caffeinate -d '
alias g='git'

#### Completion
autoload -U compinit
compinit -i -D
zmodload zsh/complist


# Add GNU completion to a function.
function gmp () {
    fcn=$1
    compdef _gnu_generic $fcn
}

# C-o accepts, but doesn't leave the menu.
# zmodload zsh/complist
bindkey -M menuselect '\C-a' accept-and-menu-complete

### Completion stuff from zsh book

# C-o accepts the menu item, but stays in the same menu to allow
# multiple selections.

zstyle ':completion:*:warnings' format 'no matches: %d'


# Necessary for the later zstyle stuff to print the descriptions.
zstyle ':completion:*:descriptions' format %B%d%b

# Group by type (command / alias / etc)
zstyle ':completion:*'  group-name ''

# Group non-parameters together
# zstyle ':completion:*:-command-:*:(commands|builtins|reserved-words|aliases)'  group-name commands

# Man pages, use sections as descriptions
zstyle ':completion:*:manuals' separate-sections true

# To get paging when too many matches

zstyle ':completion:*default' list-prompt '%S%M matches%s'
bindkey -M listscroll q send-break

zstyle ':completion:*:default' menu 'select=5'


# Turn on menu completion for tag windows (since they are a pain to type.)
# TODO: git commits?
zstyle ':completion:*:windows' menu on=0



# to delete:
# zstyle -d ':completion:*:windows' menu on=0
# to delete all styles using same context:
# zstyle -d ':completion:*:windows'


# Bind tab to complete word
bindkey '\C-i' complete-word

# Use C-x h inside complete to get debug info

# zstyle ':completion:::::' completer _expand _complete _ignored
# % echo /etc/z*
# tags in context :completion::expand:::
#     all-expansions expansions original  (_expand)

# Set the ordering of the results in the menu
zstyle ':completion:*:expand:*' tag-order 'expansions all-expansions original'

# Other options
# zstyle ':completion:*:expand:*' glob false # don't expand globs
# zstyle ':completion:*:expand:*' substitute false # don't expand $

# approximate matching
zstyle ':completion:::::' completer _expand _complete _approximate _ignored
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) )'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'

# consider lower cases as matches for upper case
# zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
# Don't use for approximate matches
zstyle ':completion:*:(^approximate)' matcher-list 'm:{a-z}={A-Z}'


setopt complete_in_word
zstyle ':completion:*:::' completer _complete _prefix
# zstyle ':completion:*:prefix:*' add-space true


#### tmux
# Run in a tmux window to reload the ssh-agent data
alias fixssh='eval $(tmux show-env -s SSH_AUTH_SOCK)'



alias d='dirs -v'
alias tc='time caffeinate'



#### Prompt
setopt prompt_subst


# # Set up a prompt.
# RPS1="%B%~%b"
# PROMPT='%B%~$(git_prompt) %n@%m
# $%b '

# Example from man zshcontrib
autoload -Uz vcs_info

zstyle ':vcs_info:(hg*|git*):*' get-revision true
zstyle ':vcs_info:(hg*|git*):*' check-for-changes true

zstyle ':vcs_info:(hg*|git*):*' stagedstr "${green}S${gray}"
zstyle ':vcs_info:(hg*|git*):*' unstagedstr "${red}U${gray}"

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' actionformats \
       '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats       \
    '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f'
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}-%r-'

zstyle ':vcs_info:git*+set-message:*' hooks git-st

# Show remote ref name and number of commits ahead-of or behind
function +vi-git-st() {
    local ahead behind remote
    local -a gitstatus

    # Are we on a remote-tracking branch?
    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
        --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} ]] ; then
        # for git prior to 1.7
        # ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
        ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
        (( $ahead )) && gitstatus+=( "${c3}+${ahead}${c2}" )

        # for git prior to 1.7
        # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
        behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
        (( $behind )) && gitstatus+=( "${c4}-${behind}${c2}" )

        hook_com[branch]="${hook_com[branch]} [${remote} ${(j:/:)gitstatus}]"
    fi
}

precmd () { vcs_info }

# PS1='%F{5}[%F{2}%n%F{5}] %F{3}%3~ ${vcs_info_msg_0_}%f
# %# '

# https://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
# # and ; were not my idea.  kate.
PS1='%B%F{6}## %D{%I:%M} %n@%m %~%f%b
; '


# Note:  To make a simple prompt (when dealing with mounted drives),
# do:
# PS1="%~ $"
# precmd()


# For emacs ansi-term
# TODO: evaluate only in ansi-term
if [ "$EMACS" ]; then
    ansi_term_chpwd() { print -P "\033AnSiTc %d" }
    chpwd_functions=(${chpwd_functions[@]} ansi_term_chpwd)
    print -P "\033AnSiTu %n"
    print -P "\033AnSiTc %d"
fi



# Use emacs client as VISUAL if already in emacs.
[[ -n $EMACS ]] && export VISUAL=emacsclient


# git co o/d/c
# fpath=(~/.dotfiles/zsh/ $fpath)
# autoload -- ~/.dotfiles/zsh/[^_]*(:t)



for f in $post_init_hook; do
    $f
done

load_if_exists $HOME/.dotfiles/local/post.zsh

zstyle ':completion:*:*:git:*' script $HOME/.zsh/git-completion.bash
fpath=($HOME/.zsh $fpath)

path=($path $HOME/.dotfiles/bin)

typeset -U path

# Uncomment for profiling
# zprof

# Generated with : pip completion --zsh
# pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip


ii() {
    if (( ${+CONDA_DEFAULT_ENV} )); then
        print -P "%Bconda%b: ${CONDA_DEFAULT_ENV}"
        print -P "%Bpython%b: $(which python)"
    fi

    if [[ ! -z ${vcs_info_msg_0_} ]]; then
        print -P "git: ${vcs_info_msg_0_}"
    fi

}
# pip zsh completion end
