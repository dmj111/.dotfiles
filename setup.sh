#!/bin/zsh

# .gitconfig

files=(
    .bash_profile
    .bashrc
    .inputrc
    .jshintrc
    .tmux.conf
)

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
print $DIR


for f in $files; do
    print $f
    rm -f ${HOME}/${f}
    src=${DIR}/${f}
    dst=${HOME}/${f}
    print $src $dst
    ln -sf ${src} ${dst}
    diff ${src} ${dst}
done

if [ ! -f $HOME/.gitconfig ]; then
    echo "writing out gitconfig"
    cat <<HEREDOC > $HOME/.gitconfig
[include]
   path = $HOME/repos/dot-files/.gitconfig
HEREDOC

fi
    
     

    
    


