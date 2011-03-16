#!/usr/bin/env bash

export PYTHOND=`pwd`
export MYPYINSTALL=$PYTHOND/python2.7

# create a directory to put source code in
mkdir -p sources
cd sources

# get the python2.7 source
wget http://www.python.org/ftp/python/2.7/Python-2.7.tgz
tar xzvf Python-2.7.tgz

# get easy install source
wget http://pypi.python.org/packages/source/d/distribute/distribute-0.6.15.tar.gz
tar zxvf distribute-0.6.15.tar.gz


cd Python-2.7
./configure --prefix=$MYPYINSTALL                    # this will print a ton of stuff and generate a Makefile
                                                     # the Makefile will install things to the directory $MYPYINSTALL
make install
cd ..

export PATH="$MYPYINSTALL/bin:$PATH"                 # put python 2.7 at the begining of your PATH

# install easy_install
cd distribute-0.6.15 && python setup.py install
cd ..

# now install pip because it's better than easy_install (haha)
easy_install pip
#pip install yolk

pip install numpy

# install IPython and it's dependencies
#pip install twisted
#pip install foolscap
#pip install pyopenssl
pip install ipython

echo "make sure that you put $MYPYINSTALL/bin at the begining of your PATH environment variable in ~/.bashrc"
echo ""
echo -e "\033[33m****************************************************************\033[0m"
echo -e "\033[31mexport PATH=\"$MYPYINSTALL/bin:\$PATH\"\033[0m"

