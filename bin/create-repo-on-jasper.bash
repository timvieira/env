#!/usr/bin/env bash

PWD=`pwd -P`
cd "$PWD" # avoid symlinks
xxx=`basename $PWD`
echo "Sending $xxx to jasper.."

# need to make sure there is a .hg in directory xxx

# use the standard hgrc file we know and love
cp `dirname $0`/jasper-hgrc .hg/hgrc

cd ..

echo "Copying files to jasper.."
rsync -r -v "$xxx" "jasper.cs.umass.edu:~/my-scratch/"

echo "Creating symlink and restart hg server"
ssh "jasper.cs.umass.edu" "cd my-scratch/hg-served-projects && ln -s ../$xxx $xxx && ./kill-server && ./start-server"

echo "Moving directory $xxx to $xxx-old"
mv "$xxx" "$xxx-old"

echo "cloning new directory"
hg clone "ssh://jasper.cs.umass.edu/my-scratch/$xxx" "./$xxx"

cd "$xxx"

echo "you might want to delete $xxx-old"
