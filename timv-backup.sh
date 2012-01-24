#!/usr/bin/env bash

set -e # abort script immediately if any command fails

# NOTES
# rysnc and slashes:
#
#   rsync -a /usr/local /media/disk
#
# creates 'local' in /media/disk, whereas
#
#   rsync -a /usr/local/ /media/disk
#
# copies all the stuff from /usr/local into disk but doesn't create 
# a parent directory -- you very rarely want to do that, so don't use
# trailing slashes on sources

TODAY=`date '+%F'`

HD="/media/gluttony"
BACKUP="$HD/backups"
NEW="$BACKUP/new"


echo "[INFO] checking for most recent backup.."
echo
echo "backups"
echo "============================"
ls -1 "$BACKUP" | sort -n
echo

if [ -e "$NEW" ]
then
    echo "[ERROR] Previous backup failed. Delete '$NEW' to continue."
    exit 1
fi

if [ -e "$BACKUP/$TODAY" ]
then
    echo "** already made a back up today **"
    exit 1
fi

if [ -e "$BACKUP/$TODAY-last" ]
then
    echo "** already made a back up today **"
    exit 1
fi

LATEST=$(echo $BACKUP/*-latest)

if [ -e $LATEST ]
then
    echo [INFO] latest back up: $LATEST
    LINKDEST=--link-dest=$LATEST
else
    echo "[INFO] There was no latest backup to link to.. creating fresh backup"
fi

if ! rsync -aR --exclude='/home/timv/.gvfs' $LINKDEST /home/timv $NEW
then
    echo "$TODAY [ERROR] Failed to create backup."
    exit 1
fi

mv "$NEW" "$BACKUP/$TODAY-latest"

if [ -e "$LATEST" ]
then
  mv "$LATEST" ${xx:0:${#xx}-7}
fi

