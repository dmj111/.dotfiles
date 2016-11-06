#!/usr/bin/env bash

BASH_DIR=$HOME/repos/dot-files/bash

for script in $BASH_DIR/*; do
    [ -x $script ]  || continue
    . $script
done


