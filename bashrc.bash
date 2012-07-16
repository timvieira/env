# ~/.bashrc: executed by bash(1) for non-login shells.

#______________________________________________________________________________
# Environment variables

if [ -e ~/jdk1.6.0_31/bin ]; then
    export PATH=$PATH:~/jdk1.6.0_31/bin   # local install
    export JAVA_HOME=~/jdk1.6.0_31
else
    export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64/
fi

# prepend to path environment variable
function add-path () {
    for d in `echo $@`; do
        export PATH=$d:$PATH
    done
}

PROJECTS=~/projects
JAVAEXTRAS=$PROJECTS/extras/java

# The Path
add-path $JAVA_HOME/bin
add-path ~/inst/bin                   # local install
add-path $PROJECTS/env/bin            # my misc scripts
add-path ~/software/visualvm_133/bin
add-path ~/software/eclps/bin/x86_64_linux   # ECLiPSe constraint solver
add-path ~/software/ziboptsuite-2.1.1/*/bin  # zimpl, scip

# CPLEX license file
export ILOG_LICENSE_FILE=~/software/CPLEX/access.ilm

# Python
export PYTHONPATH=$PROJECTS:$PROJECTS/extras/python:$PROJECTS/incubator:$PROJECTS/shelf:$PYTHONPATH

# Classpath
export CLASSPATH=.:$CLASSPATH

# Temporarily add all jars in directory to classpath
function classpath-hack {
    jars=`find -name '*.jar'`
    cp=${jars//[[:space:]]/:}
    echo $cp:$CLASSPATH
    export CLASSPATH=$cp:$CLASSPATH
}

# Jython
export JYTHON_HOME=$JAVAEXTRAS/jython
export CLASSPATH=$JYTHON_HOME/jython.jar:$CLASSPATH
alias jython="java -mx3G -cp target -jar $JYTHON_HOME/jython.jar"

# Emacs is my preferred editor, dammit!
export EDITOR=emacs
export HGEDITOR='emacs -nw'

function pkill9 {
  kill -9 `pgrep $@`
}

# ls aliases
alias ll='ls -lAh'
alias la='ls -A'
alias l='ls -CF'
alias lll='ls -h -l --group-directories-first --ignore=*.pyc --ignore=*.o --ignore=*.class' # --ignore-backup
alias ls-recent='ls -lAt'

# cd aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

alias v='visit'

#-------------

# vanilla emacs
alias emacs-plain='shutup-and-disown emacs --no-init-file --no-splash'
alias visualvm='shutup-and-disown visualvm'

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
export HISTIGNORE="&:ls:[bf]g:exit:clear"
export HISTTIMEFORMAT='%F %T '

shopt -s cmdhist

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

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
    bind "set completion-ignore-case on"
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

# Compress the cd, ls -l series of commands.
function cl {
   if [ $# = 0 ]; then
      cd && l
   else
      cd "$*" && ll
   fi
}

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

function find-with-ignores {
    find . \( -name '*.class' -o -name '*.jar' -o -name '.hg' \) -prune -o -type f
}

alias find-big-files="find . -type f -exec ls -s {} \; | sort -n -r"

#______________________________________________________________________________
# Clean up

function tmpfiles {
  find -name '*~'
}

function pyclean {
  rm -f `find . -name "*.pyc"`
  rm -f `find . -name "*$py.class"`
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

function edit-exec {
    visit `which $@`
}

function t {
    if [[ "$#" -ne 1 ]]; then  # list files
        ll ~/Dropbox/todo/*
    else
        find ~/Dropbox/todo -type f -name "*$1*" -exec visit {} \;
    fi
}

# edit configuration files with env project
function e {
    ENV=~/projects/env
    if [[ "$#" -ne 1 ]]; then
        cd $ENV
    else
        visit `find $ENV |grep -iv '.hg\|emacs-support\|bin' |grep $1`
    fi
}


function p {
    # calling with no arguments lands you in the projects directory.
    if [[ "$#" -eq 0 ]]; then
        cd $PROJECTS
        return
    fi
    allmatches=`find $PROJECTS -path '*'$1'*' -type d `
    for proj in $allmatches; do
        for repo in `find $proj -type d -path '*/working/.hg'`; do
            cd $repo; cd ..
            return
        done
        for repo in `find $proj -type d -name '.hg'`; do
            cd $repo; cd ..
            return
        done
    done
    # second attempt
    for proj in `find $PROJECTS -path '*'$1'*' -type d`; do
        cd $proj
        return
    done
    red "failed to find match for project $1"
}

function write-stuff {
    f=`find ~/projects -type f |grep '\.\(tex\|org\|md\)$' | grep -v '\.\(hg\|git\|svn\)' | grep $1`
    echo $f
    cd `dirname $f`
    v $f
    o *.pdf
}

# Bash Directory Bookmarks
#alias m1='alias g1="cd `pwd`"'
#alias m2='alias g2="cd `pwd`"'
#alias m3='alias g3="cd `pwd`"'
#alias m4='alias g4="cd `pwd`"'
#alias m5='alias g5="cd `pwd`"'
#alias m6='alias g6="cd `pwd`"'
#alias m7='alias g7="cd `pwd`"'
#alias m8='alias g8="cd `pwd`"'
#alias m9='alias g9="cd `pwd`"'
#alias mdump='alias|grep -e "alias g[0-9]"|grep -v "alias m" > ~/.bookmarks'
#alias lma='alias | grep -e "alias g[0-9]"|grep -v "alias m"|sed "s/alias //"'
#touch ~/.bookmarks
#source ~/.bookmarks

alias ldp='cd ~/projects/ldp/code/working'
alias lpldp='cd ~/projects/ldp/code/working/lpldp'
alias sso='cd ~/projects/courses/stochastic-opt/project'

#______________________________________________________________________________
# Shortcuts for annoying deep directories (like Java source code).
#
# TODO: add smare ignores like ack (e.g. *.class .hg/* .cvs/*)

# find files LIKE $1 and open them in emacs
function fv {
    find-and-apply $1 visit
}

# find file LIKE $1 and then call $2
function find-and-apply {
    $2 `find src/ -name "*$1*"`
}

#______________________________________________________________________________
# Python tricks

# Show pythonpath
alias pypath="python -c 'import sys; print sys.path' | tr ',' '\n' | grep -v 'egg'"

# cd to the directory containing specified python module
function cdpy {
    cd `python -c "import os; import $1; print os.path.dirname($1.__file__)"`
}

# edit python module by name
function vpy {
    python -m debug.edit "$@"
}

#______________________________________________________________________________
# Misc bash function

function o {
    # gnome-open; xdg-open    # unity equivalent of gnome-open
    xdg-open $@ 2>/dev/null
}

# like nohup
function shutup-and-disown {
    CMD="$@"
    $CMD 2>/dev/null &
    disown $! 2>/dev/null >/dev/null   # $! is most recent PID
}

function push-public-key {
    publickey=`cat ~/.ssh/id_rsa.pub`
    # make sure you set the appropriate permissions!
    ssh "$1" "mkdir -p ~/.ssh/ && touch .ssh/authorized_keys && chmod 600 .ssh/authorized_keys && echo $publickey >> .ssh/authorized_keys && cat .ssh/authorized_keys"
}

alias tetris='google-chrome /home/timv/public_html/tetris.swf 2>/dev/null'

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

function todos {
    ack 'TODO|XXX|FIXME|FIX|timv|TIMV|HACK|REFACTOR|BROKEN' $@;
}

function red    { echo -e "\e[31m$@\e[0m"; }
function yellow { echo -e "\e[33m$@\e[0m"; }
function green  { echo -e "\e[32m$@\e[0m"; }
function blue   { echo -e "\e[34m$@\e[0m"; }
function purple { echo -e "\e[35m$@\e[0m"; }
function cyan   { echo -e "\e[36m$@\e[0m"; }

# kill a process after a number of seconds
# usage:
#    doalarm <seconds to wait> program arg arg ...
function doalarm { perl -e 'alarm shift; exec @ARGV' "$@"; }

# list top commands in bash history
function top-commands () {
    history |linepy 'print " ".join(line.split()[3:])' | sort | uniq -c | sort -rn
}

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
      *.bz2)       bunzip2 $1    ;;
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
