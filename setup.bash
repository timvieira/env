#!/usr/bin/env bash

cd `dirname $0`

function add-link {
    local absp=$(ls -d -1 $PWD/$1)
    local base=`basename $absp`
    local dest="~/.$base"
    if [ -f $dest ]; then
        ln -s $absp $dest
    else
        echo "Skipping $dest, already exists"
    fi
}

add-link bashrc
add-link pylintrc
add-link screenrc
add-link ackrc
add-link aspell.en.pws
add-link emacs/emacs.el
add-link hgrc

if [ -f ~/texmf ]; then
    ln -s `pwd`/texmf ~/texmf
fi

#sudo apt-get install python-dev python-setuptools python-numpy python-scipy
#sudo apt-get install mercurial git
#sudo easy_install ipython
#sudo easy_install hg-git
#sudo apt-get install texlive-latex-base
#apt-cache search ttf-bitstream-vera
