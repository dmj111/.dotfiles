# echo "in .bashrc"

BASH_DIR=$HOME/repos/dot-files/bash

# Load all executable scripts in BASH_DIR
for script in $BASH_DIR/*; do
    [ -x $script ]  || continue
    . $script
done


