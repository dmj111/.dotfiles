# Dotfiles

Setup:

    . bootstrap.sh

creates an alias, sets option to ignore untracked, checksout


## Directories

- dotfiles/
  - dmj-tools -- python package with user tools
  - local-example


## Using pipx/pre-commit/flake8

- add ~/.local/bin to path in the local config
- install some libraries

        python3 -m pip install --user --force pip
        pip install --user pipx
        pipx install pre-commit
        pipx install conan
        pipx install flake8

- use `pre-commit install` to install pre-commit in a repo


- install the custom tools

        pipx install $HOME/dotfiles/dmj-tools


TODO: editable install doesn't seem to install the standalone scripts
