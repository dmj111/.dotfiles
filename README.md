# Dotfiles

    cd $HOME
    git clone https://github.com/dmj111/.dotfiles.git
    cd .dotfiles
    ./setup.sh


## Directories

- bin -- scripts
- dmj-tools -- python package with user tools
- home/ -- files/folder to link into ~/


## Using pipx/pre-commit/flake8

- add ~/.local/bin to path in the local config
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
