#!/usr/bin/env bash

# ~/.bashrc: executed by bash(1) for non-login shells.

# pass aliases through sudo ... don't know if the really works...
#alias sudo="sudo " # space triggers alias substitution

#______________________________________________________________________________
# Environment variables

# prepend to path environment variable
function add-path {
    for d in `echo $@`; do
        export PATH=$d:$PATH
    done
}
function add-pypath {
    for d in `echo $@`; do
        export PYTHONPATH=$d:$PYTHONPATH
    done
}
function add-classpath {
    for d in `echo $@`; do
        export CLASSPATH=$d:$CLASSPATH
    done
}

if [ -e ~/jdk1.6.0_31/bin ]; then
    add-path ~/jdk1.6.0_31/bin   # local install
    export JAVA_HOME=~/jdk1.6.0_31
else
    export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64/
fi


PROJECTS=~/projects
JAVAEXTRAS=$PROJECTS/extras/java

# The Path
add-path $JAVA_HOME/bin
add-path ~/inst/bin                   # local install
add-path $PROJECTS/env/bin            # my misc scripts
add-path ~/software/visualvm_133/bin
add-path ~/software/eclps/bin/x86_64_linux   # ECLiPSe constraint solver
add-path ~/software/ziboptsuite-2.1.1/*/bin  # zimpl, scip
add-path ~/.cabal/bin                        # Haskell executables

# CPLEX license file
export ILOG_LICENSE_FILE=~/software/CPLEX/access.ilm

# Python
add-pypath \
    $PROJECTS \
    $PROJECTS/arsenal \
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
export JYTHON_HOME=$JAVAEXTRAS/jython
add-classpath $JYTHON_HOME/jython.jar
alias jython="java -mx3G -cp target -jar $JYTHON_HOME/jython.jar"

#export EDITOR=emacs
export EDITOR=visit
export HGEDITOR='emacs -nw'

function pkill9 {
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


#--------------------------
# potentially useful
alias grepall="grep -RIn" # recursive grep on non-binary files; a lot like ack

# highlighter
function hl() {
  grep -E --color=always $1'|$'
}

# grep paragraphs
function grepp {
  pattern=$1
  file=$2
  awk 'BEGIN{RS="";ORS="\n\n";FS="\n"}/'$pattern'/' $file | hl $pattern
}

#--------------------------


#-------------

# vanilla emacs
alias emacs-plain='shutup-and-disown emacs --no-init-file --no-splash'
alias visualvm='shutup-and-disown visualvm'

alias serve='o http://localhost:8000 && python -m SimpleHTTPServer'


############################################################
# If not running interactively, don't do anything
[ -z "$PS1" ] && return
############################################################

#______________________________________________________________________________
# Keybindings

bind "'\C-o': '\C-e 2>&1 |less -R'"      # append "2>&1 |less" to end of line
bind "'\C-f': '\C-ustty sane\n\r\C-l'"   # some times terminal get broken...

#______________________________________________________________________________
# Bash History

# don't put duplicate lines in the history. See bash(1) for more options don't
# overwrite. GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=10000000000
export HISTFILESIZE=100000000000
export HISTIGNORE="&:ls:[bf]g:exit:clear:pwd:ll"
export HISTTIMEFORMAT='%F %T '

shopt -s cmdhist


# list top commands in bash history
function top-commands () {
    history |linepy 'print " ".join(line.split()[3:])' | freq
}


# TOOD: last_command function

#######
# Augmented bash history with metadta (~/.bash_history_meta), such as the
# working directory command was executed.
#
# TODO:
#
#  * PROMPT_COMMAND is only called after the command executes. What we actually
#    need is a pre-execute hook.
#
#  * some commands don't get written in history (e.g. ls, pwd
#
#  * might "break" if there are two shells writting to history
#
function prompt_command {

  # timv: this seems to write to bash history
  history -a

  CMD=`history 1`  # much faster than `history |tail -n1`

  # pull command number out of history file; $HISTCMD didn't work..
  HISTNUM=`echo "$CMD" |cut -f1 -d' '`

  if [[ "$PREV_HISTNUM" -ne "$HISTNUM" ]]; then
      if [ ! -z "$PREV_DIRECTORY" ]; then
          DIR="$PREV_DIRECTORY"
          echo "$DIR $CMD" >> ~/.bash_history_metadata
      fi
  fi

  export PREV_DIRECTORY="$PWD"
  export PREV_HISTNUM="$HISTNUM"
}

PROMPT_COMMAND="prompt_command"

# view bash history for this directory
function dir-history {
    cat ~/.bash_history_metadata |grep "^$PWD "
}

function dir-history-list {
    dir-history | linepy 'print re.sub("^\\S+\\s+\\S+\\s+\\S+\\s+\\S+ ", "", line)'
}

# note: this is not an accurate count because store succesive repeats of an
# identical command.
function dir-history-common {
    dir-history-list |freq
}

#______________________________________________________________________________
#

# check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# make less friendlier for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned off
# by default to not distract the user: the focus in a terminal window should be
# on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

# Awesome prompt ideas: http://maketecheasier.com/8-useful-and-interesting-bash-prompts/2009/09/04
#PROMPT_COMMAND='PS1="\[\033[0;33m\][\!]\`if [[ \$? = "0" ]]; then echo "\\[\\033[32m\\]"; else echo "\\[\\033[31m\\]"; fi\`[\u.\h: \`if [[ `pwd|wc -c|tr -d " "` > 18 ]]; then echo "\\W"; else echo "\\w"; fi\`]\$\[\033[0m\] "; echo -ne "\033]0;`hostname -s`:`pwd`\007"'


# http://www.gnu.org/software/bash/manual/bashref.html
if [ "$color_prompt" = yes ]; then
    # prints user@host:cwd$
    #PS1='\[\033[01;32m\]\u@\h\[\033[00m\]\$ '
    PS1='${debian_chroot:+($debian_chroot)}\[\033[03;32m\]\u@\h\[\033[00m\] \[\033[03;34m\]\w\[\033[00m\]\n\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Enable bash completion.
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
    bind "set completion-ignore-case on"   # case insensitive
fi


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

alias less='less -RSimw'
export PAGER='less -RSimw'

# ssh aliases
#alias gargamel='ssh tvieira2@gargamel.cs.uiuc.edu'
#alias smeagol='ssh tvieira2@smeagol.cs.uiuc.edu'
#alias jasper='ssh timv@jasper.cs.umass.edu'
#alias vinci8='ssh timv@vinci8.cs.umass.edu'
#alias dali='ssh timv@dalisrv.cs.umass.edu'
#alias loki='ssh timv@loki.cs.umass.edu'
alias ugradx='ssh timv@ugradx.cs.jhu.edu'
alias clsp='ssh timv@login.clsp.jhu.edu'

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

function ff {
    find $2 -type f -iname '*'$1'*' |ignore-filter
}

function ignore-filter {
    grep -v '\(.class\|.pyc\)$' |grep -v '.hg\|.git'
}

# open filenames matching pattern. By default, look in the `src/` directory
# (specify `.` as second argument for old behavior).
function fv {
    pattern="$1"
    directory="$2"
    if [[ "$#" -eq "0" ]]; then
        return
    fi
    if [[ "$#" -eq "1" ]]; then
        directory="src"
    fi
    matches=`f "$pattern" "$directory" | ignore-filter`
    echo "$matches"
    echo "$matches" |xargs v
}

#______________________________________________________________________________
# Clean up

function tmpfiles {
    find $1 -name '*~'
}

function pyclean {
    rm -f `find . -name "*.pyc"`
    rm -f `find . -name "*$py.class"`
}

# TODO: remove executables associate with file... e.g. hw4.hs has {hw4.hi, hw4.0, hw4}
function haskell-clean {
    rm -f `find -name "*.hi"`
    rm -f `find -name "*.o"`
}

# remove org-mode's LaTeX output files
function org-clean {
    rm -f `org-export-files`
}

function org-export-files {
    find -name '*.pdf' |xargs ack --files-with-matches 'Creator\(Emacs Org-mode version 7.8.03\)'
    find -name '*.tex' |xargs ack --files-with-matches 'pdfcreator={Emacs Org-mode version 7.8.03}}'
}

# clean up tex derived files
function tex-clean {
    rm -f *.log *.aux *.blg *.bbl *.dvi
}

# clean up derived files.
function clean {
    tex-clean
    org-clean
    pyclean
    haskell-clean
}

#______________________________________________________________________________
# Version control tricks

function find-repos {
    find ~/ -name ".hg" -type d -exec dirname {} \;
    find ~/ -name ".git" -type d -exec dirname {} \;
}

# try to find repositories which have changes which might need to be pushed
function hg-changed-repos {
    cd
    repos=`find -name '.hg' -type d -exec dirname {} \;`
    for line in $repos; do
        cd $line
        echo -n "$line -- "
        MODIFIED=$(hg st -m)
        if [ "$MODIFIED" != "" ]; then
            red modified
        else
            outgoing=$(doalarm 3 hg outgoing |grep "no changes found")
            if [[ $outgoing != "no changes found" ]]; then
                cyan outgoing
            else
                yellow ok
            fi
        fi
        cd ~
    done
}

## hg for sandboxed repos
#alias hg="sudo -u vc hg"
#alias hg-chown-vc="sudo chown -R vc .hg; sudo chgrp -R vc .hg"
#alias hg-chown-me="sudo chown -R timv .hg; sudo chgrp -R timv .hg"

# more on log formatting http://hgbook.red-bean.com/read/customizing-the-output-of-mercurial.html
alias hgtree="hg log --template '{rev} {node|short} {author|user}: {desc} ({date|age})\n'"
alias hgchangelog="hg log --style changelog"
alias hgserve="o http://localhost:8000 && hg serve"   # serve and open

# run pop open kdiff3 and open editor
function hg-diff-ci {
    for f in $(hg st -m -n $(hg root)); do   # use relative paths
        echo $f
        hg kdiff3 $f 2>/dev/null &
        hg ci $f
    done
}
alias gittree='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20 %s %cr"'
alias gittree-who='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20 %cn %s %cr"'
alias gittree-when='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20 %cn %s %ci"'
##
#function get_git_branch {
#    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\ \[\1\]/'
#}
#PS1="\h:\W \u\[\033[0;32m\]\$(get_git_branch) \[\033[0m\]\$ "
##

#______________________________________________________________________________
# Shortcuts for jump around

alias source-bashrc='source ~/.bashrc'
alias sb='source-bashrc'
alias edit-script='es'

# TODO: (low priority) bash completion for things on path!
# TODO: (low priority) fall-back for aliases?
function es {
    if [[ "$#" -eq "0" ]]; then  # list files
        v ~/.bashrc
        return 0
    fi
    # try using which.
    wh=`which $@`
    if [[ "$wh" ]]; then
        v "$wh"
    else
        edit-bash-function "$@"
    fi
}


# turn on bash's extended debugging options
shopt -s extdebug


# Edit file defining some bash function; we'll even jump to the line number.
function edit-bash-function {

    # we'll need to temporarily enable bash's extended debugging
    shopt -s extdebug

    out=`declare -F "$@"`

    if [[ -z "$out" ]]; then

        if [[ $(alias $1) ]]; then
            echo "'$@' appears to be an alias. You're on your own for this one."
            return 1
        fi

        echo "failed to find source for '$@'."

        return 1
    fi

    echo $out

    # convert output into a bash array
    array=(`echo "$out"`)
    lineno=${array[1]}
    filename=${array[2]}

    # open file at lineno with visit
    visit +$lineno:0 "$filename"

    # Turn off extended shell debugging
#    shopt -u extdebug
}

function t {
    if [[ "$#" -ne 1 ]]; then  # list files
        tree ~/Dropbox/todo/
    else
        files=`find ~/Dropbox/todo -type f -name "*$1*.org*" |grep -v '\.org_archive$' |ignore-filter`
        for f in `echo $files`; do
            v $f
        done
    fi
}

# edit configuration files with env project
function e {
    ENV=~/projects/env
    if [[ "$#" -ne 1 ]]; then
        cd $ENV
    else
        files="$(find $ENV |grep -iv '.hg\|site-lisp\|bin')
$(ls -x $ENV/emacs/*.el)"
        files=$(echo "$files" |grep $1)
        echo "$files" |grep $1           # only for highlighting
        v $files
    fi
}


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

    projname=$(echo $PROJECTS/*/working $PROJECTS/*/*/working $PROJECTS/* |sed 's/ /\n/g')

    # courses
    courses=`find $PROJECTS/courses -type d`

    # version controlled project roots -- be sure to strip off hg directories or
    # else they'll get filtered out
    vcroots=`find $PROJECTS -name '.hg' -type d | grep -v incoming | grep -v '/projects/notes/' |sed 's/\\/\.hg$//g'`

    # sort vc roots so that prefixes come first
    vcroots=`echo "$vcroots" |sort`

    # everything else
    everythingelse=`find $PROJECTS -type d`

    matches="$projname
$courses
$vcroots
$everythingelse"

    # first filter things inside .hg directories
    matches=`echo "$matches" | grep -v '\.hg'`

    #echo "$matches" |filter.py $@

    matches=`echo "$matches" |filter.py -c $@`

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

alias ldp='cd ~/projects/ldp/code/working'
alias lpldp='cd ~/projects/ldp/code/working/lpldp'
alias arsenal='cd ~/projects/arsenal'


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

function push-public-key {
    publickey=`cat ~/.ssh/id_rsa.pub`
    # make sure you set the appropriate permissions!
    ssh "$1" "mkdir -p ~/.ssh/ && touch .ssh/authorized_keys && chmod 600 .ssh/authorized_keys && echo $publickey >> .ssh/authorized_keys && cat .ssh/authorized_keys"
}

alias tetris='shutup-and-disown google-chrome /home/timv/public_html/tetris.swf 2>/dev/null'

function jhu-library {
    o "http://proxy.library.jhu.edu/login?url=$1"
}

function ghetto-refresh {
    if [[ "$#" -ne "2" ]]; then
        echo "ghetto-refresh <rate> <cmd>"
        return
    fi
    while [ 1 ]; do
        echo `$2`
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

function find-note-files {
    find "$@" -type f -name 'TODO*' -o -name 'NOTE*' -o -name 'LOG*' -o -name "*.tex" -o -name "*.org" \
      |grep -v '~\|#'  \
      |grep -v '/export/' \
      |grep -v 'site-lisp' \
      |grep -iv '\.\(pdf\|log\)$'  # lets assume we want to edit the notes, not view
}

function org-export-filter {
    ack --files-without-matches 'pdfcreator={Emacs Org-mode version 7.8.03}}' $@
}

## # filters are keyword prefix matches (e.g. 'dd bp' matches 'bp-ddecomp'; but
## # 'dsl' does not match 'bdslss')
## function find-notes {
##     files="$(ls -t1 `find-note-files ~/projects`) $(ls -t1 `find-note-files ~/Dropbox`)"
##
##     if [[ "$#" -eq "0" ]]; then
##         green "Ten most recent files:"
##
##         # remove org-export files -- apply this filter after others because it
##         # is slower (it looks at the content of the file).
##         files=`org-export-filter $files`
##
##         ls -t $files | head -n20
##     else
## #        for filter in "$@"; do
## #            files=`echo "$files" | grep -i "\b$filter"`
## #        done
##
##         files=`echo "$files"| filter.py $@`
##
##         # remove org-export files -- apply this filter after others because it
##         # is slower (it looks at the content of the file).
##         files=`org-export-filter $files`
##
##         echo "$files"
##     fi
## }

function notes {
   matches=`find-note-files ~/projects |filter.py $@`

   retcode="$?"

   # TODO: search skid as well
   echo "$matches"

   if [[ "$retcode" -eq "0" ]]; then
       # feeling lucky, so we'll open the file for you.

       # drop color codes
       match=`echo "$matches" |pysed '\\033\[.*?m' ''`

       cd `dirname $match`

       o "$match"

   else
       yellow "pick a file or be more specific."

   fi


}


alias remove-empty-lines='grep -v "^\s*$"'

# TODO: if there is an org file and other junk; ignore the other junk.
#function notes {
#    notes="$(find-notes "$@")"
#    if [[ `echo "$notes" |remove-empty-lines| wc -l` -eq "0" ]]; then
#        red "no results.";
#        return
#    fi
#    if [[ `echo "$notes" |wc -l` -eq "1" ]]; then
#        cd $(dirname $notes)
#        v "$notes"
#    else
#        echo "$notes"
#    fi
#}

#function notes-dir {
#    cd $(dirname $(find-notes "$@"))
#}

# grep notes for patterns
# TODO: generalize to keyword search
function notes-ack {
    find-note-files ~/projects | xargs ack -ai "$@"
    find-note-files ~/Dropbox | xargs ack -ai "$@"
    find-note-files ~/.skid | xargs ack -ai "$@"
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

function extract {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xvjf $1   ;;
      *.tar.gz)    tar xvzf $1   ;;
#      *.bz2)       bunzip2 $1    ;;
      *.bz2)       tar xjfv $1    ;;
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

# concatenate pdfs
function concat-pdfs {
  out='output.pdf'
  gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE="$out" -dBATCH "$@"
  echo "wrote to $out"
}


#_______________________________________________________________________________
#

# optcomplete harness for bash shell. You then need to tell
# bash to invoke this shell function with a command like
# this::
#
#   complete -F _optcomplete <program>
#
_optcomplete()
{
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        OPTPARSE_AUTO_COMPLETE=1 $1 ) )
}

# work-in-progress general solution to bash_history-based completion
_histcomplete()
{
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        OPTPARSE_AUTO_COMPLETE=1 hist-complete.py notes ) )
}


#alias skid='python -m skid'
alias skid-dir='cd `python -c "import skid.config as c; print c.ROOT"`'

complete -F _optcomplete skid

#complete -F _optcomplete notes.py
complete -F _histcomplete notes
