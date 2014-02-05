#!/usr/bin/env bash

# ~/.bashrc: executed by bash(1) for non-login shells.

# pass aliases through sudo ... don't know if the really works...

ENV=~/projects/env
PROJECTS=~/projects
JAVAEXTRAS=$PROJECTS/extras/java

source $ENV/bash/util/path.bash


#______________________________________________________________________________
# Environment variables

export BIBINPUTS=/home/timv/projects/env/timv.bib:$BIBINPUTS


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

# add all jars in directory to classpath (not permanent)
function add-jars-to-classpath {
    jars=`find -name '*.jar'`
    add-classpath $jars
    echo "$CLASSPATH"
}

# Jython
#export JYTHON_HOME=$JAVAEXTRAS/jython
#add-classpath $JYTHON_HOME/jython.jar
#alias jython="java -mx3G -cp target -jar $JYTHON_HOME/jython.jar"

#export EDITOR=emacs
export EDITOR=visit
export HGEDITOR='emacs -nw'
export GIT_EDITOR=$HGEDITOR

function pkill9 {
  ps aux |grep "$@"
  kill -9 `pgrep $@`
}

# ls aliases
alias ll='ls -lAh'
alias la='ls -A'
alias l='ls -CF'
alias lll='ls -h -l --group-directories-first --ignore=*.pyc --ignore=*.o --ignore=*.class' # --ignore-backup
alias lt='ls -lAt'

# cd aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias dl='cd ~/Downloads'
alias de='cd ~/Desktop'

# order lines by frequency (most frequent first).
alias freq='sort | uniq -c |sort -nr'
alias remove-empty-lines='grep -v "^\s*$"'
alias space2newline="sed 's/ /\n/g'"


#-------------

# vanilla emacs
alias emacs-plain='shutup-and-disown emacs --no-init-file --no-splash'
#alias visualvm='shutup-and-disown visualvm'
alias serve='o http://localhost:8000 && python -m SimpleHTTPServer'

#______________________________________________________________________________
# Bash History

# don't put duplicate lines in the history. See bash(1) for more options don't
# overwrite.
export HISTCONTROL=ignoredups:ignoreboth:ignorespace

# cmdhist: If set, Bash attempts to save all lines of a multiple-line command in
#    the same history entry. This allows easy re-editing of multi-line commands.
shopt -s cmdhist

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=10000000000
export HISTFILESIZE=100000000000
export HISTIGNORE="&:ls:[bf]g:exit:clear:pwd:ll"
export HISTTIMEFORMAT='%F %T '

# append to the history file, don't overwrite it
shopt -s histappend

# globstar: If set, the pattern '**' used in a filename expansion context will
#    match all files and zero or more directories and subdirectories. If the
#    pattern is followed by a '/', only directories and subdirectories match.
shopt -s globstar


# list top commands in bash history
function top-commands () {
    history |linepy 'print " ".join(line.split()[3:])' | freq
}


source $ENV/bash/dir-history.bash

#______________________________________________________________________________
#

# check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# make less friendlier for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

source $ENV/bash/prompt.bash

#______________________________________________________________________________
# Aliases

# Colorization aliases
# enable color support of several utils
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias less='less -R'
    alias ack='ack --color --group'
    alias tree='tree -C'
fi

alias tree='tree -I "*.pyc"'

alias less='less -RSimw'
export PAGER='less -RSimw'

#______________________________________________________________________________
# bash functions

# use +1GB for file larger than 1 gig.
function find-files-by-size {
    find -size "$1" -exec ls -lh {} \;
}

#export FIND_IGNORES="-name '*.class' -o -name '*.jar' -o -name '.hg' -o -name '*.pyc'"
#function find-with-ignores {
#    find . \( $FIND_IGNORES \) -prune -o -type f
#}

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
# Clean up

source $ENV/bash/cleanup.bash

#______________________________________________________________________________
# Version control tricks

source $ENV/bash/util/version-control.bash


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

function o {
    # gnome-open; xdg-open    # unity equivalent of gnome-open
    xdg-open "$@" 2>/dev/null >/dev/null
}

alias tetris='shutup-and-disown google-chrome /home/timv/tetris.swf 2>/dev/null'

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

function org-export-filter {
    ack --files-without-matches 'pdfcreator={Emacs Org-mode' $@
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

# misc aliases
#alias difflr="diff -B --expand-tabs --side-by-side"
#alias poweroff-display='sleep 1 && xset dpms force off'

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

#______________________________________________________________
# Audio Conversion
function m4a2wav {
    for i in *.m4a; do
        mplayer -ao pcm "$i" -ao pcm:file="${i%.m4a}.wav"
    done
}

function wav2mp3 {
    for i in *.wav; do
        lame -h -b 192 "$i" "${i%.wav}.mp3"
    done
}

function m4a2mp3 {
    m4a2wav
    wav2mp3
    #rm *.wav
    echo "There are probably some temporary wav files you can delete."
    echo "Currenly, m4a2mp3 will *not* delete these for you."
}
#______________________________________________________________________________
#

# convert {ppt, odf} to pdf
alias to-pdf='libreoffice --headless --invisible --convert-to pdf'

# convert djvu to pdf
function djvu2pdf {
    ddjvu -format=pdf -quality=85 -verbose "$1" "$1.pdf"
}

# concatenate pdfs
function concat-pdfs {
    out='output.pdf'
    gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE="$out" -dBATCH "$@"
    echo "wrote to $out"
}


#_______________________________________________________________________________
#

#alias skid='python -m skid'
alias skid-dir='cd `python -c "import skid.config as c; print c.ROOT"`'

source $ENV/bash/my-complete.bash

alias gnome-do-restart='(pkill9 gnome-do && shutup-and-disown gnome-do) >& /dev/null'


_complete_bibkeys ()
{
    X="/home/timv/.skid/bibkeys"
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        hist-complete.py $X ) )
}
alias bibkeys='python -m skid.utils.bibkeys complete'
complete -F _complete_bibkeys   bibkeys


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

function graphviz {
    out=$1.svg
    green output file: $out
    cat $1 |dot -Tsvg > $out
    shutup-and-disown "google-chrome $out"
}

function nocolor {
    python -c 'import sys, re; [sys.stdout.write(re.sub("\033\[[0-9;]*m","",x)) for x in sys.stdin]'
}

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

# TODO: remove newline characters
#function paste-bash-command {
#   xsel |tr '\n' '\\' |xsel -i | xdotool click 2
#}


function notes {

    # TODO: search skid as well.
    # TODO: don't just use filename. include the title (heuristic, first line=title)

    COMP_NOTES=/home/timv/projects/notes/.index/files

    matches=`cat $COMP_NOTES |bymtime - |cut -f2 |/home/timv/projects/env/bin/filter.py $@`
    retcode="$?"

    echo "$matches"

    if [[ "$retcode" -eq "0" ]]; then
        # feeling lucky, so we'll open the file for you.

        # drop color codes
        match=`echo "$matches" |pysed '\\033\[.*?m' '' `

        cd `dirname $match`

        # dispatch to the appropriate opener; the text editor is the default
        if [[ "$match" =~ .*\.(nb)$ ]]; then
            gnome-open $match
        elif [[ "$match" =~ .*\.(ipynb)$ ]]; then
            ipython notebook --pylab inline $match
        else
            $EDITOR "$match"
        fi

        bash   # sigh. Changing directory worked for bash function, but not for
               # this script version...

    else
        yellow "pick a file or be more specific."

    fi
}


source $ENV/bash/util/ssh.bash
