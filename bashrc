#!/usr/bin/env bash

ENV=~/projects/env
PROJECTS=~/projects

# globstar: If set, the pattern '**' used in a filename expansion context will
#    match all files and zero or more directories and subdirectories. If the
#    pattern is followed by a '/', only directories and subdirectories match.
shopt -s globstar

# check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize


source $ENV/bash/util/path.bash

source $ENV/bash/history.bash
source $ENV/bash/alias.bash


#______________________________________________________________________________
# Environment variables

#export BIBINPUTS=/home/timv/projects/env/timv.bib:$BIBINPUTS

if [ -e ~/jdk1.6.0_31/bin ]; then
    add-path ~/jdk1.6.0_31/bin   # local install
    export JAVA_HOME=~/jdk1.6.0_31
else
    export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64/
fi


# The Path
add-path $JAVA_HOME/bin
add-path ~/inst/bin                   # local install
add-path $PROJECTS/env/bin            # my misc scripts
#add-path ~/software/visualvm_133/bin
add-path ~/software/eclps/bin/x86_64_linux   # ECLiPSe constraint solver
add-path ~/software/ziboptsuite-2.1.1/*/bin  # zimpl, scip
add-path ~/.cabal/bin                        # Haskell executables

# CPLEX license file
#export ILOG_LICENSE_FILE=~/software/CPLEX/access.ilm
export ILOG_LICENSE_FILE=~/software/cplex/access.ilm

# Python
add-pypath \
    $PROJECTS \
    $PROJECTS/ldp/code/working/lpldp \
    $PROJECTS/incubator \
    $PROJECTS/shelf \
    $PROJECTS/shelf/quantities \
    ~/software/OpenCV-2.4.2/release/lib \

# Classpath
add-classpath .

#_______________________________________________________________________________
#

export EDITOR=visit
export PAGER='less -RSimw'
export HGEDITOR='emacs -nw'
export GIT_EDITOR=$HGEDITOR

#_______________________________________________________________________________
#

function pkill9 {
  ps aux |grep "$@"
  kill -9 `pgrep $@`
}

# order lines by frequency (most frequent first).
alias freq='sort | uniq -c |sort -nr'

#_______________________________________________________________________________
#

alias remove-empty-lines='grep -v "^\s*$"'
alias space2newline="sed 's/ /\n/g'"

function nocolor {
    python -c 'import sys, re; [sys.stdout.write(re.sub("\033\[[0-9;]*m","",x)) for x in sys.stdin]'
}

#_______________________________________________________________________________
#

# vanilla emacs
alias emacs-plain='shutup-and-disown emacs --no-init-file --no-splash'
alias serve='o http://localhost:8000 && python -m SimpleHTTPServer'

#______________________________________________________________________________
#

# timv: seems to happens by default now
# make less friendlier for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


#______________________________________________________________________________
# Find stuff in current directory:

# use +1GB for file larger than 1 gig.
function find-files-by-size {
    find -size "$1" -exec ls -lh {} \;
}

alias find-big-files="find . -type f -exec ls -s {} \; | sort -n -r"

#______________________________________________________________________________
# Shortcuts for annoying deep directories (like Java source code).

# grep filenames recursive file listing
function f {
    find $2 -type f |ignore-filter |grep -i "$1"
}

#function ff {
#    find $2 -type f -iname '*'$1'*' |ignore-filter
#

# TODO: should probably filter all hidden directories.
function ignore-filter {
    grep -v '\(.class\|.pyc\|.o\|.hi\)$' |grep -v '\.hg\|\.git\|\.ipynb_checkpoints\|build'
}

# fv ("flexible visit" or "find and visit") recursively searches for a file path
# matching specified pattern. Opens the file if a unique match is found.
function fv {
    find -type f | ignore-filter | bymtime - | cut -f2 | filter.py $@ --on-unique 'v {match}'
}

#______________________________________________________________________________
# Shortcuts for jumping around

source $ENV/bash/quick-edit.bash

# open todo lists
function t {

    if [[ "$#" -eq 0 ]]; then
        visit "~/Dropbox/todo/todo.org"
        return
    fi

    files=`find ~/Dropbox/todo -type f |grep -v '\.org_archive$' |ignore-filter`

    matches=`echo "$files" |filter.py "$@" --on-unique 'visit {match}'`

    retcode="$?"

    echo "$matches"
}

# Edit configuration files in the env project
function e {
    if [[ "$#" -eq 0 ]]; then
        yellow $ENV
        cd $ENV
        return
    fi
    files="$(find $ENV |grep -iv '.hg\|site-lisp')
$(ls -x $ENV/emacs/*.el)"
    matches=`echo "$files" | ignore-filter |filter.py $@ --on-unique 'v {match}'`
    echo "$matches"
}


# Jump to project directory
function p {
    # calling with no arguments lands you in the projects directory.
    if [[ "$#" -eq 0 ]]; then
        cd $PROJECTS
        return
    fi

    # TODO: in order to get some shortcircuiting/lazy evaluation, break the
    # search process up by project sources {courses, vcroots, everythingelse}
    # rather than accumulating the lists up front as we do here, make separate
    # calls to a utility which tries to find a match on success goes there -
    # short-circuiting the search process.

    # TODO: cache the output of these functions try searching the cache first -
    # whenever we find something which is "broken" or "missing" we can
    # regenerate it.

    # TODO: add directories of recently modified files to the list of things we
    # search

    # TODO: utility which searches recent files from the command-line (such a
    # tool must already exist!)

    matches=`cat $COMP_PROJECTS`

    #echo "$matches" |filter.py $@

    matches=`echo "$matches" |filter.py -C $@`

    # TODO: repos with more overlap with name should come first
    # e.g.
    #     $ p pdfhacks
    #
    # should prefer ~/projects/pdfhacks over
    #  ~/projects/pdfhacks/bibtex-as-annotation, but doesn't at the moment
    #  simply because of directory ordering.

    for d in $matches; do
        # might want to iterate thru this set..
        cd "$d"
        return
    done
    red "failed to find match for project $1"
}

#______________________________________________________________________________
# Python tricks

# ack thru all the python code on my hard drive
function pysearch {
    locate -0 '*.py' |xargs -0 ack --color --group "$@"
}

# Show pythonpath
alias pypath="python -c 'import sys; print sys.path' | tr ',' '\n' | grep -v 'egg'"

# cd to the directory containing specified python module
function cdpy {
    cd `python -c "import os; import $1; print os.path.dirname($1.__file__)"`
}

# edit python module by name
function vpy {
    python -m arsenal.debug.edit "$@"
}

#______________________________________________________________________________
# Misc bash function

# deprecated: use browser extension instead.
#function jhu-library {
#    o "http://proxy.library.jhu.edu/login?url=$1"
#}

function ghetto-refresh {
    if [[ "$#" -ne "2" ]]; then
        echo "ghetto-refresh <rate> <cmd>"
        return
    fi
    while [ 1 ]; do
        $2
        sleep $1
        clear
    done
}

#_______________________________________________________________________________
# Finding notes quickly

# todo: sometimes I like to make a bulleted list of TODO items... how can we
# grab those?
function todos {
    ack -i '(TODO|XXX|FIXME|FIX|timv|HACK|REFACTOR):|XXX[ ]' $@
}

# grep notes for patterns
# TODO: generalize to multiple keyword search (filter by multiple regexps)
function notes-ack {
    cat $COMP_NOTES | xargs ack -i "$@"
    echo ~/.skid/marks/*.d/notes.org | xargs ack -i "$@"
}

#_______________________________________________________________________________
#

function red    { echo -e "\e[31m$@\e[0m"; }
function yellow { echo -e "\e[33m$@\e[0m"; }
function green  { echo -e "\e[32m$@\e[0m"; }
function blue   { echo -e "\e[34m$@\e[0m"; }
function purple { echo -e "\e[35m$@\e[0m"; }
function cyan   { echo -e "\e[36m$@\e[0m"; }

# kill a process after a number of seconds
# usage: doalarm <seconds to wait> program arg arg ...
function doalarm { perl -e 'alarm shift; exec @ARGV' "$@"; }

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# print one file on remove server "loki.cs.umass.edu"
#function print-loki {
#  PFROM="loki.cs.umass.edu"
#  PNAME="woper-dbl"
#  for f in $@; do
#    BASENAME=$(basename "$f")
#    scp "$f" "$PFROM:~/tmp/$BASENAME"
#    ssh "$PFROM" "lpr -P$PNAME ~/tmp/$BASENAME"
#    echo
#  done
#}

function compare-lines {
    hs=""
    for f in `echo $@`; do
        h=/tmp/`pyhash $f`
        hs="$hs $h"
        cat $f |sort > $h   # timv: option for uniq (i.e. set difference)?
    done
    kdiff3 $hs
}

function compare-uniq-lines {
    hs=""
    for f in `echo $@`; do
        h=/tmp/`pyhash $f`
        hs="$hs $h"
        cat $f |sort |uniq > $h
    done
    kdiff3 $hs
}

alias pyhash="python -c 'import sys, hashlib; print hashlib.sha1(str().join(sys.argv[1:]) or raw_input()).hexdigest()'"


#_______________________________________________________________________________
# Generally useful utils for working with filepaths

function mtime {
    python -c "
from datetime import datetime
from path import path
print datetime.fromtimestamp(path('$1').mtime)"
}


function filter-org-export {
    python -c "
import os, sys

for f in sys.stdin:
    f = f.strip()

    if not os.path.exists(f):
        continue

    if f.endswith('.tex'):
        # filter org-mode tex export.
        if any(('Emacs Org-mode version' in l) for l in file(f)):
            continue

    if f.endswith('.pdf'):
        # filter org-mode pdf export.
        if any(('Creator(Emacs Org-mode version' in l) for l in file(f)):
            continue
    print f
"
}

function filter-file-exists {
    python -c "
import os, sys

for f in sys.stdin:
    f = f.strip()

    if not os.path.exists(f):
        continue

    print f
"
}

#_______________________________________________________________________________
# wrappers around other programs

function graphviz {
    out=$1.svg
    green output file: $out
    cat $1 |dot -Tsvg > $out
    shutup-and-disown "google-chrome $out"
}

alias gnome-do-restart='(pkill9 gnome-do && shutup-and-disown gnome-do) >& /dev/null'

function extract {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xvjf $1   ;;
      *.tar.gz)    tar xvzf $1   ;;
      *.bz2)       tar xjfv $1   ;;
      *.rar)       unrar x $1    ;;
      *.gz)        gunzip $1     ;;
      *.tar)       tar xvf $1    ;;
      *.tbz2)      tar xvjf $1   ;;
      *.tgz)       tar xvzf $1   ;;
      *.zip)       unzip $1      ;;
      *.Z)         uncompress $1 ;;
      *.7z)        7z x $1       ;;
      *)           echo "'$1' cannot be extracted via >extract<" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

function o {
    # gnome-open; xdg-open    # unity equivalent of gnome-open
    xdg-open "$@" 2>/dev/null >/dev/null
}

alias tetris='shutup-and-disown google-chrome /home/timv/tetris.swf 2>/dev/null'

#_______________________________________________________________________________
#

alias poweroff-display='sleep 1 && xset dpms force off'

function disable-touchpad {
    xinput list \
        |grep -i touchpad \
        |linepy '
[x] = re.findall("id=(\d+)", line)
os.system("xinput set-prop %s \"Device Enabled\" 0" % x)'
}

function enable-touchpad {
    xinput list \
        |grep -i touchpad \
        |linepy '
[x] = re.findall("id=(\d+)", line)
os.system("xinput set-prop %s \"Device Enabled\" 1" % x)'
}

#_______________________________________________________________________________
#

source $ENV/bash/notes.bash
source $ENV/bash/skid.bash
source $ENV/bash/my-complete.bash
source $ENV/bash/cleanup.bash
source $ENV/bash/prompt.bash

source $ENV/bash/util/ssh.bash
source $ENV/bash/util/pdf.bash
source $ENV/bash/util/audio.bash
source $ENV/bash/util/version-control.bash
