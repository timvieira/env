#!/usr/bin/env bash

cd `dirname $0`

function add-link2 {
    local absp=$(ls -d -1 $PWD/$1)
    local dest=$2
    if [ -e $dest ]; then
        echo "Skipping $dest, already exists"
    else
        ln -s $absp $dest
        echo "added $dest"
    fi
}

function add-link {
    local absp=$(ls -d -1 $PWD/$1)
    local base=`basename $absp`
    add-link2 $1 ~/.$base
}

add-link bashrc
add-link pylintrc
add-link screenrc
add-link ackrc
add-link aspell.en.pws
add-link emacs/emacs.el
add-link hgrc
add-link gitconfig
add-link inputrc
add-link notesrc

if [ ! -f ~/texmf ]; then
    echo "Skipping ~/texmf, already exists."
else
    echo "added ~/texmf"
    ln -s `pwd`/texmf ~/texmf
fi

# no dot in front of this destination.
add-link2 texmf ~/texmf

add-link2 ipython_config.py ~/.ipython/profile_default/ipython_config.py

#mkdir -p ~/.ipython/extensions/
#mkdir -p ~/.ipython/profile_default/

#sudo easy_install ipython
#sudo easy_install hg-git
#sudo apt-get install texlive-latex-base
#apt-cache search ttf-bitstream-vera
