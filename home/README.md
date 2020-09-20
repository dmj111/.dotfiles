# Dotfiles

- Use setup.sh to get going.

- use "conda init zsh" to get the config settings for the local installation, move to ~/.local-dotfiles/.zshrc


- add ~/.local/bin to path

    python3 -m pip install --user --force pip
    pip install --user pipx
    pipx install pre-commit
    pipx install conan
    pipx install flake8

- use `pre-commit install` to install pre-commit in a repo


- Or install in editable mode

    pipx install -e --spec $HOME/.dotfiles/dmj-tools dmj-tools
