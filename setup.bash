#!/usr/bin/env bash

cd `dirname $0`

function add-link {
    local absp=$(ls -d -1 $PWD/$1)
    local base=`basename $absp`
    local dest=~/.$base
    if [ -f $dest ]; then
        echo "Skipping $dest, already exists"
    else
        ln -s $absp $dest
        echo "added $dest."
    fi
}

add-link bashrc
add-link pylintrc
add-link screenrc
add-link ackrc
add-link aspell.en.pws
add-link emacs/emacs.el
add-link hgrc
add-link gitconfig

if [ ! -f ~/texmf ]; then
    echo "Skipping ~/texmf, already exists."
else
    echo "added ~/texmf"
    ln -s `pwd`/texmf ~/texmf
fi

#sudo apt-get install python-dev python-setuptools python-numpy python-scipy
#sudo apt-get install mercurial git
#sudo easy_install ipython
#sudo easy_install hg-git
#sudo apt-get install texlive-latex-base
#apt-cache search ttf-bitstream-vera
