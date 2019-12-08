# Dotfiles

- Use setup.sh to get going.
- install pyenv with pyenv-installer
- python3 -m pip install --user pipx  (probably from system python?)
- add ~/.local/bin to path
- pipx install pre-commit
- use `pre-commit install` to install in a repo
- pipx reinstall-all $(pyenv which python)


    pipx install --spec $HOME/.dotfiles/packages/clean_git clean-git
    pipx upgrade --spec $HOME/.dotfiles/packages/clean_git clean-git
