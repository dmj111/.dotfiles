# Dotfiles


## Setup

These commands will link the config files from this repository into
`$HOME`.

    cd $HOME
    git init --separate-git-dir=.dotfiles.git
    git remote add upstream git@github.com:dmj111/.dotfiles.git
    git fetch
    git checkout -b keep-changes
    git reset upstream/main
    git add -u
    git commit -m "Keep changes"
    mv .git .gitdir
    alias dgit="git --git-dir $HOME/.dotfiles.git --work-tree=$HOME"



### Setting local settings for email/user

This is mostly for working on main.  A local branch should put this
info in .local.gitconfig


    git config --local user.name <name>
    git config --local user.email <email>



## Directories

- bin -- bash scripts
- dmj-tools -- python package with user tools
- home/ -- files/folder to link into ~/


## Using pipx/pre-commit/flake8

- install some libraries
        python3 -m pip install --user --force pip
        pip install --user pipx
        pipx install pre-commit
        pipx install autopep8
        pipx install flake8

- autopep8, conan, cookiecutter, dmj-tools
  flake8, kaggle, mp3-utils (mine, stored somewhere)

- use `pre-commit install` to install pre-commit in a repo


- install the custom tools

        pipx install $HOME/.dotfiles/dmj-tools


TODO: editable install doesn't seem to install the standalone scripts

## Conda settings

conda config --describe

to disable conda modifying the prompt:

conda config --set changeps1 false
