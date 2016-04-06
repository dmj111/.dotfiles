# README

## To use

In `.zshrc`, put something like this:


    ZSH=${HOME}/repos/zsh
    source ${ZSH}/init.zsh


To make local customizations, create `$ZSH/local/local.zsh`, and add functions to the
lists `pre_init_hook`, and `post_init_hook` to be called from init.zsh.
