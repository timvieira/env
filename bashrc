# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#______________________________________________________________________________
# Keybindings

# appends "|less" to the end of current line
bind "'\C-o': '\C-e |less'"

#______________________________________________________________________________
# Bash History

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=10000000000
export HISTFILESIZE=100000000000
export HISTIGNORE="&:ls:[bf]g:exit:clear"

shopt -s cmdhist

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

#______________________________________________________________________________
# 

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
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
    #PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    #print user@host$
    #PS1='\[\033[01;32m\]\u@\h\[\033[00m\]\$ '
#    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\w\[\033[00m\]\n\$ '
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
fi

#______________________________________________________________________________
# Environment variables

# Emacs is my preferred editor, dammit!
export EDITOR=emacs
export HGEDITOR=emacs

# Java
export JAVA_HOME=/usr/lib/jvm/java-6-sun

# The Path
export PATH=$PATH:$JAVA_HOME/bin
export PATH=$PATH:~/projects/bin

# Python
export PYTHONPATH=/home/timv/projects/python-extras:/home/timv/projects:$PYTHONPATH
export PYTHONSTARTUP=/home/timv/projects/python-extras/pythonstartup.py


JAVAEXTRAS=/home/timv/projects/java-extras
export CLASSPATH=.:$CLASSPATH

# Learning-Based Java (LBJ)
LBJDIR=$JAVAEXTRAS/lbj-things
export CLASSPATH=$CLASSPATH:$LBJDIR/LBJPOS.jar:$LBJDIR/LBJ2.jar:$LBJDIR/LBJ2Library.jar

# Jython
export JYTHON_HOME=$JAVAEXTRAS/jython
export CLASSPATH=$JYTHON_HOME/jython.jar:$CLASSPATH

# Redstone xml-rpc
R=$JAVAEXTRAS/redstone-xmlrpc
export CLASSPATH=$CLASSPATH:$R/simple-4.0.1.jar:$R/simple-xmlrpc-1.0.jar:$R/xmlrpc-1.1.1.jar


# Apache xml-rpc
A=$JAVAEXTRAS/apache-xmlrpc-3.1.2/lib/
export CLASSPATH=$A/commons-logging-1.1.jar:$A/ws-commons-util-1.0.2.jar:$CLASSPATH
export CLASSPATH=$A/xmlrpc-common-3.1.2.jar:$A/xmlrpc-client-3.1.2.jar:$A/xmlrpc-server-3.1.2.jar:$CLASSPATH

# Ubigraph-Java Client
export CLASSPATH=$JAVAEXTRAS/ubigraph.jar:$CLASSPATH


# bash will check all the directories in the $CDPATH list for matches to the directory name.
# export CDPATH='.:~:/usr/local/apache/htdocs:/disk/backups'

# XXX: THIS MIGHT NOT BE NECESSARY:
export LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib

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
fi

# ls aliases
alias ll='ls -lAh'
alias la='ls -A'
alias l='ls -CF'
alias lll='ls -h -l --group-directories-first --ignore=*.pyc --ignore=*.class' # --ignore-backup

# cd aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# ssh aliases
alias gargamel='ssh tvieira2@gargamel.cs.uiuc.edu'
alias jasper='ssh timv@jasper.cs.umass.edu'


#______________________________________________________________________________
# bash functions

function emacsd() {
    ps -C emacs >/dev/null && return 1
    emacs $@ 2>&1 >/dev/null &
    disown `pgrep emacs`  # get emacs PID and detach it from this shell.
}

function visit() {
    ( emacsd $@ 2>&1 >/dev/null ) || ( emacsclient $@ 2>&1 >/dev/null & )
}

function pyclean() {
    rm -f `find . -name "*.pyc"`
    rm -f `find . -name "*$py.class"`
}

# print one file on remove server "loki.cs.umass.edu"
function print-loki() {
    PFROM="loki.cs.umass.edu"
    PNAME="woper-dbl"
    BASENAME=$(basename $1)
    scp $1 $PFROM:~/tmp/$BASENAME
    ssh $PFROM "lpr -P$PNAME ~/tmp/$BASENAME"
}

function extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1        ;;
            *.tar.gz)    tar xvzf $1     ;;
            *.bz2)       bunzip2 $1       ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xvf $1        ;;
            *.tbz2)      tar xvjf $1      ;;
            *.tgz)       tar xvzf $1       ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1    ;;
            *)           echo "'$1' cannot be extracted via >extract<" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}


#______________________________________________
#

NO_COLOUR="\[\033[0m\]"
LIGHT_WHITE="\[\033[1;37m\]"
WHITE="\[\033[0;37m\]"
GRAY="\[\033[1;30m\]"
BLACK="\[\033[0;30m\]"

RED="\[\033[0;31m\]"
LIGHT_RED="\[\033[1;31m\]"
GREEN="\[\033[0;32m\]"
LIGHT_GREEN="\[\033[1;32m\]"
YELLOW="\[\033[0;33m\]"
LIGHT_YELLOW="\[\033[1;33m\]"
BLUE="\[\033[0;34m\]"
LIGHT_BLUE="\[\033[1;34m\]"
MAGENTA="\[\033[0;35m\]"
LIGHT_MAGENTA="\[\033[1;35m\]"
CYAN="\[\033[0;36m\]"
LIGHT_CYAN="\[\033[1;36m\]"

function EXT_COLOR () { echo -ne "\033[38;5;$1m"; }

# set a fancy prompt
#export PS1="${RED}[\u@\h \W]\$${NO_COLOUR} "
# set a fancy prompt
#export PS1="`EXT_COLOR 172`[\u@\h \W]\$${NO_COLOUR} "


#pronounce() {
#    wget -qO- $(wget -qO- "http://www.m-w.com/dictionary/$@" | grep 'return au' | sed -r "s|.*return au\('([^']*)', '([^'])[^']*'\).*|http://cougar.eb.com/soundc11/\2/\1|") | aplay -q; 
#}

