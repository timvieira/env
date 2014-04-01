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

source $ENV/bash/dir-history.bash

# list top commands in bash history
function top-commands {
    history |linepy 'print " ".join(line.split()[3:])' | freq
}

function h {
    history |grep "$@"
}

source $ENV/bash/dir-history.bash
