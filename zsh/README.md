# README

## To use

In `.zshrc`, put something like this:


    ZSH=${HOME}/repos/zsh
    source ${ZSH}/init.zsh


To make local customizations, create `$ZSH/local/local.zsh`, and add functions to the
lists `pre_init_hook`, and `post_init_hook` to be called from init.zsh.

Something like:

    function local_pre() {
        path=($HOME/bin
              /usr/local/bin
              $path)
    }

    pre_init_hook+=local_pre

    function local_post () {
        RPS1=""
        fpath=($fpath /usr/local/share/zsh-completions)
    }

    post_init_hook+=local_post



To get bash to redirect to zsh, use this in .bashrc:

    function run_zsh {
        zsh_exe=$1
        [ -f ${zsh_exe} ] && exec ${zsh_exe} -l
    }
    run_zsh /bin/zsh
    run_zsh /usr/bin/zsh

