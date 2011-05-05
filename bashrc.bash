# ~/.bashrc: executed by bash(1) for non-login shells.

#______________________________________________________________________________
# Environment variables

PROJECTS=~/projects
JAVAEXTRAS=$PROJECTS/java-extras
export SCALA_HOME=~/projects/scala

# The Path
export PATH=$PATH:$JAVA_HOME/bin:$SCALA_HOME/bin
export PATH=$PATH:~/projects/bin

# Python
export PYTHONPATH=$PROJECTS/python-extras:$PROJECTS/incubator:$PROJECTS:$PYTHONPATH

# Classpath
export CLASSPATH=.:$CLASSPATH

# Learning-Based Java (LBJ)
LBJDIR=$JAVAEXTRAS/lbj
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

# Emacs is my preferred editor, dammit!
export EDITOR=emacs
export HGEDITOR='emacs -nw'

function pkill9 () {
  kill -9 `pgrep $@`
}

# ls aliases
alias ll='ls -lAh'
alias la='ls -A'
alias l='ls -CF'
alias lll='ls -h -l --group-directories-first --ignore=*.pyc --ignore=*.o --ignore=*.class' # --ignore-backup

# cd aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

############################################################
# If not running interactively, don't do anything
[ -z "$PS1" ] && return
############################################################

#______________________________________________________________________________
# Keybindings

# appends "2>&1 |less" to the end of current line
bind "'\C-o': '\C-e 2>&1 |less'"
bind "'\C-f': '\C-ustty sane\n\r\C-l'"

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
export HISTTIMEFORMAT='%F %T '

#______________________________________________________________________________
# 

shopt -s cmdhist

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


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

alias less='less -RSimw'
export PAGER='less -RSimw'

# ssh aliases
alias gargamel='ssh tvieira2@gargamel.cs.uiuc.edu'
alias smeagol='ssh tvieira2@smeagol.cs.uiuc.edu'
alias jasper='ssh timv@jasper.cs.umass.edu'
alias vinci8='ssh timv@vinci8.cs.umass.edu'
alias dali='ssh timv@dalisrv.cs.umass.edu'
alias loki='ssh timv@loki.cs.umass.edu'

# misc aliases
#alias difflr="diff -B --expand-tabs --side-by-side"
#alias poweroff-display='sleep 1 && xset dpms force off'

alias gittree='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20 %s %cr"'
alias gittree-who='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20 %cn %s %cr"'
alias gittree-when='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20 %cn %s %ci"'

#______________________________________________________________________________
# bash functions

# use +1GB for file larger than 1 gig.
function find-files-by-size () {
  find -size "$1" -exec ls -lh {} \;
}

alias findbig="find . -type f -exec ls -s {} \; | sort -n -r | head -5"


#function say { mplayer -really-quiet "http://translate.google.com/translate_tts?tl=en&q=$1"; }

function idea () {
  $JAVAEXTRAS/idea-IC-95.66/bin/idea.sh &
  disown `pgrep -f "com\.intellij\.idea\.Main"`
}

function pyclean() {
  rm -f `find . -name "*.pyc"`
  rm -f `find . -name "*$py.class"`
}

alias gcal='python -m gcal.quickadd'

# print one file on remove server "loki.cs.umass.edu"
function print-loki() {
  PFROM="loki.cs.umass.edu"
  PNAME="woper-dbl"
  for f in $@; do
    BASENAME=$(basename "$f")
    scp "$f" "$PFROM:~/tmp/$BASENAME"
    ssh "$PFROM" "lpr -P$PNAME ~/tmp/$BASENAME"
    echo
  done
}

function extract () {
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

#______________________________________________________________
# Audio Conversion
function m4a2wav () {
  for i in *.m4a; do
    mplayer -ao pcm "$i" -ao pcm:file="${i%.m4a}.wav"
  done
}

function wav2mp3 () {
  for i in *.wav; do
    lame -h -b 192 "$i" "${i%.wav}.mp3"
  done
}

function m4a2mp3 () {
  m4a2wav 
  wav2mp3
  #rm *.wav
  echo "There are probably some temporary wav files you can delete."
  echo "Currenly, m4a2mp3 will *not* delete these for you."
}

#______________________________________________________________
#
