#!/usr/bin/env bash


PWD=`pwd -P`
cd "$PWD" # avoid symlinks
xxx=`basename $PWD`
echo "send $xxx to jasper"

# need to make sure there is a .hg in directory xxx

# use the standard hgrc file we know and love
cp `dirname $0`/jasper-hgrc .hg/hgrc

cd ..
rsync -r -v $xxx jasper.cs.umass.edu:~/my-scratch/

# ln ../$xxx $xxx
ssh jasper.cs.umass.edu "cd my-scratch/hg-served-projects && ln -s ../$xxx $xxx && ./kill-server && ./start-server"

echo
echo "Now, you need to clone this thing"
echo "hg clone http://jasper.cs.umass.edu:8000/$xxx ./$xxx"
echo