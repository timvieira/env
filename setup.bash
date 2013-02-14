#!/usr/bin/env bash

cd `dirname $0`

function add-link {
   absp=$(ls -d -1 $PWD/$1)
   base=`basename $absp`
   ln -s $absp ~/.$base
}

add-link bashrc
add-link pylintrc
add-link screenrc
add-link ackrc
add-link aspell.en.pws
add-link emacs/emacs.el

#sudo apt-get install python-dev python-setuptools python-numpy python-scipy
#sudo apt-get install mercurial git
#sudo easy_install ipython
#sudo easy_install hg-git
#sudo apt-get install texlive-latex-base
#apt-cache search ttf-bitstream-vera
