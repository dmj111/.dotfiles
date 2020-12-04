# Dotfiles

    cd $HOME
    git clone https://github.com/dmj111/.dotfiles.git
    cd .dotfiles
    ./setup.sh


## Directories

- bin -- scripts
- dmj-tools -- python package with user tools
- home/ -- files/folder to link into ~/


## Using pipx/pre-commit
- add ~/.local/bin to path

    python3 -m pip install --user --force pip
    pip install --user pipx
    pipx install pre-commit
    pipx install conan
    pipx install flake8

- use `pre-commit install` to install pre-commit in a repo


- Or install in editable mode

    pipx install -e --spec $HOME/dmj-tools dmj-tools
