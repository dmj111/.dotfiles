# Dotfiles

    cd $HOME
    git clone https://github.com/dmj111/.dotfiles.git
    cd .dotfiles
    git config core.worktree ../../
    # This step will wipe out any existing changes, careful...
    git reset --hard origin/main


## Directories

- home/bin -- scripts
- home/dmj-tools -- python package with user tools
- .zsh/
- .emacs.d

## Git commands

From inside the .dotfiles dir, run commands like this:

- git ls-files ../
- git grep zsh ../
- git add -f ../new-file


## Using pipx/pre-commit
- add ~/.local/bin to path

    python3 -m pip install --user --force pip
    pip install --user pipx
    pipx install pre-commit
    pipx install conan
    pipx install flake8

- use `pre-commit install` to install pre-commit in a repo


- Or install in editable mode

    pipx install -e --spec $HOME/home/dmj-tools dmj-tools
