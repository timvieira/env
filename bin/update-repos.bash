#!/usr/bin/env bash

REPO=$1

function die () {
  red "failed pushing changes to \"$REPO\"."
#  exit 1
}

function green {
  echo -e "\033[1;32m$1\033[0m"
}

function red {
  echo -e "\033[1;31m$1\033[0m"
}


if [ $REPO ]; then

  echo
  green "updating jasper..."
  hg push http://jasper.cs.umass.edu:8000/$REPO || die

  echo
  green "updating git..."  
  hg push git+ssh://git@github.com:timvieira/$REPO.git || die

  echo
  green "updating bitbucket..."
  hg push https://timv@bitbucket.org/timv/$REPO || die

else
  echo "what is the name of the repo?"
  exit 1
fi


