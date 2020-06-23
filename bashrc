#!/usr/bin/env bash

# TODO:
#
#  Cute idea for adhoc bash "help"
#
#   - show previous invocations of a command (maybe the output?)
#
#   - include any comments written about bash functions, e.g. comments directly
#     above the function definition in the source code.
#

ENV=~/projects/env
PROJECTS=~/projects

export PATH=~/.local/bin:$PATH

# globstar: If set, the pattern '**' used in a filename expansion context will
#    match all files and zero or more directories and subdirectories. If the
#    pattern is followed by a '/', only directories and subdirectories match.
shopt -s globstar

# check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize


if tty -s; then
    # interactive
    bind -f ~/.inputrc
fi

source $ENV/bash/quick-edit.bash
source $ENV/bash/notes.bash
source $ENV/bash/skid.bash
source $ENV/bash/my-complete.bash
source $ENV/bash/cleanup.bash
source $ENV/bash/prompt.bash
source $ENV/bash/alias.bash
source $ENV/bash/history.bash  # deps on alias
source $ENV/bash/projects.bash
source $ENV/bash/find.bash
source $ENV/bash/misc.bash
source $ENV/bash/python.bash

source $ENV/bash/util/path.bash
source $ENV/bash/util/ssh.bash
source $ENV/bash/util/pdf.bash
source $ENV/bash/util/audio.bash
source $ENV/bash/util/version-control.bash

# enabled default bash completions
if [ -f /etc/bash_completion ]; then
 . /etc/bash_completion
fi

#______________________________________________________________________________
# Environment variables

#export BIBINPUTS=/home/timv/projects/env/timv.bib:$BIBINPUTS

#if [ -e ~/jdk1.6.0_31/bin ]; then
#    add-path ~/jdk1.6.0_31/bin   # local install
#    export JAVA_HOME=~/jdk1.6.0_31
#else
#    export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64/
#fi

# The Path
add-path $ENV/bin            # my scripts
add-path ~/.cabal/bin                        # Haskell executables

# CPLEX license file
export ILOG_LICENSE_FILE=~/software/cplex/access.ilm

# Python
add-pypath \
    $PROJECTS/incubator \
    $PROJECTS/shelf \
    $PROJECTS \
    $PROJECTS/ldp/code/working/lpldp \
    $PROJECTS/shelf/quantities \
    ~/software/OpenCV-2.4.2/release/lib

# Classpath
add-classpath .

#_______________________________________________________________________________
#

export EDITOR=visit
export PAGER='less -RSimw'
export HGEDITOR='emacs -nw'
export GIT_EDITOR=$HGEDITOR

#______________________________________________________________________________
#
# filter lines containing filepaths I am rarely interested in -- mainly
# compiled/binary file extensions (e.g., *.pyc, *.o) and version control
# directories.
#
# TODO: consider filtering all hidden directories.
# TODO: directory filter skips substring matches
function ignore-filter {
    grep -v '\(\.class\|\.pyc\|\.o\|\.hi\|\.so\|__pycache__\)$' \
      |grep -v '\(__init__\.py\)' \
      |grep -v '\(\.hg\|\.svn\|\.git\|egg-info\|\.ipynb_checkpoints\)\b' \
      |grep -v '\(build/\|dist/\|tmp/\|output/\|data/\|coverage-report\|third-party/\|results.*/\)' \
      |grep -v '\(\.prof$\|\.fls\|\.fdb_latexmk\)' \
      |grep -v '\(#.*#\|.*~$\)'
}

#______________________________________________________________________________
# Shortcuts for jumping around

# open todo lists
function t {

    if [[ "$#" -eq 0 ]]; then
        visit "~/Dropbox/todo/todo.org"
        return
    fi

    files=`find ~/Dropbox/todo |grep -v '\.org_archive$' |ignore-filter |grep -v '\.pdf$'`

    matches=`echo "$files" |filter.py "$@"`
    retcode="$?"

    if [[ $retcode -eq 0 ]]; then
        v `echo $matches | nocolor`
    fi

    echo "$matches"
}

# Edit configuration files in the env project
function e {
    if [[ "$#" -eq 0 ]]; then
        yellow $ENV
        cd $ENV
        return
    fi
    files="$(find $ENV |grep -iv '.hg\|site-lisp\|texmf')
$(ls -x $ENV/emacs/*.el)"
    matches=`echo "$files" | ignore-filter |filter.py $@ --on-unique 'v {match}'`
    echo "$matches"
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
    cat $COMP_NOTES | xargs -0 ack -i "$@"
    echo ~/.skid/marks/*.d/notes.org | xargs -0 ack -i "$@"
}

#_______________________________________________________________________________
#


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



#alias mtime='python -c "
#import sys
#from datetime import datetime
#from path import path
#print datetime.fromtimestamp(path(sys.argv[1]).mtime)"'

#function filter-org-export {
#    python -c "
#import os, sys
#
#for f in sys.stdin:
#    f = f.strip()
#
#    if not os.path.exists(f):
#        continue
#
#    if f.endswith('.tex'):
#        # filter org-mode tex export.
#        if any(('Emacs Org-mode version' in l) for l in file(f)):
#            continue
#
#    if f.endswith('.pdf'):
#        # filter org-mode pdf export.
#        if any(('Creator(Emacs Org-mode version' in l) for l in file(f)):
#            continue
#    print f
#"
#}
#
#function filter-file-exists {
#    python -c "
#import os, sys
#
#for f in sys.stdin:
#    f = f.strip()
#
#    if not os.path.exists(f):
#        continue
#
#    print f
#"
#}

function _cpufreak {
    gov=$1  # TODO: check that valid governor is passed in.
    for cpu in `echo 0 1 2 3 4 5 6 7`; do  # TODO: automatically enumerate CPUs.
        echo "cpu $cpu" '> was:' `cat /sys/devices/system/cpu/cpu$cpu/cpufreq/scaling_governor`
        sudo cpufreq-set -g $gov -c$cpu
        echo "     " '> now:' `cat /sys/devices/system/cpu/cpu$cpu/cpufreq/scaling_governor`
    done
    cat /proc/cpuinfo |grep MHz
}

alias cpuinfo='cat /proc/cpuinfo |grep MHz'
alias cpufreak='_cpufreak performance'
alias fast='_cpufreak performance'
alias slow='_cpufreak powersave'

#export LD_PRELOAD=/usr/local/lib/libfst.so:$LD_PRELOAD

alias bibgrep="locate '*.bib' |xargs ack -i"


# Standard places to do local install (Chandler's convention)
#export PATH="$HOME/.local/bin:$PATH"
#export PATH="$HOME/bin:$PATH"
#export CPLUS_INCLUDE_PATH="$HOME/include:$CPLUS_INCLUDE_PATH"
#export C_INCLUDE_PATH="$HOME/include:$C_INCLUDE_PATH"
#export LIBRARY_PATH="$HOME/lib:$LIBRARY_PATH"
#export LD_LIBRARY_PATH="$HOME/lib:$LD_LIBRARY_PATH"

# Wrapper around make, which covers building different project types, when an
# actual Makefile isn't present.
function my-make {
    if [[ -e Makefile ]]; then
        #yellow "[make] found Makefile"
        /usr/bin/make $@
    else
        #yellow "[make] No Makefile found"
        if [[ -e setup.py ]]; then
            #yellow "[make] python setup.py build_ext -i"
            python setup.py build_ext -i
        else
            # Compile most-recently modified tex file, if one exists.
            local tex=`((cat .latexmk) || (ls -t *.tex 2>/dev/null)) |head -n1`
            if [[ -n $tex ]]; then
                #yellow "[make] latexmk $tex"
                # The following option is required for the `minted` package
                # -latex="pdflatex -shell-escape %O %S"
                latexmk -interaction=nonstopmode -f -pdf $tex
                if [[ "$#" -eq 0 ]]; then
                     local pdf=`echo $tex |sed -e 's/tex$/pdf/'`
                     o $pdf
                fi
            fi
        fi
    fi
}
alias make='my-make'

# Gurobi optimization software
#export GUROBI_HOME="/home/timv/Downloads/gurobi752/linux64"
#export GUROBI_HOME="/home/timv/Downloads/gurobi8.0.0_linux64/gurobi800/linux64"
export GUROBI_HOME="/home/timv/Downloads/gurobi8.1.1_linux64/gurobi811/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash


# added by Anaconda3 installer
export PATH="/home/timv/anaconda3/bin:$PATH"
# added by Anaconda3 2018.12 installer
# >>> conda init >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$(CONDA_REPORT_ERRORS=false '/home/timv/anaconda3/bin/conda' shell.bash hook 2> /dev/null)"
if [ $? -eq 0 ]; then
    \eval "$__conda_setup"
else
    if [ -f "/home/timv/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/timv/anaconda3/etc/profile.d/conda.sh"
        CONDA_CHANGEPS1=false conda activate base
    else
        \export PATH="/home/timv/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda init <<<
