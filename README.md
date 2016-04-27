# README

## To use

In `.zshrc`, put something like this:


    ZSH=${HOME}/repos/zsh
    source ${ZSH}/init.zsh


To make local customizations, create `$ZSH/local/local.zsh`, and add functions to the
lists `pre_init_hook`, and `post_init_hook` to be called from init.zsh.


To get bash to redirect to zsh:

    function run_zsh {
        zsh_exe=$1
        [ -f ${zsh_exe} ] && exec ${zsh_exe} -l
    }
    run_zsh /bin/zsh
    run_zsh /usr/bin/zsh

