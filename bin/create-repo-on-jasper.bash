#!/usr/bin/env bash


PWD=`pwd`
xxx=`basename $PWD`
echo "send $xxx to jasper"

cd ..
scp -r $xxx jasper.cs.umass.edu:~/my-scratch/

# ln ../$xxx $xxx
ssh jasper.cs.umass.edu "cd my-scratch/hg-served-projects && ln -s ../$xxx $xxx && ./kill-server && ./start-server"

echo
echo "Now, you need to clone this thing"
echo "hg clone http://jasper.cs.umass.edu:8000/$xxx ./$xxx"
echo