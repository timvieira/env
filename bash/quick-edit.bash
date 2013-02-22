#
# Utilities for the compulsive bash configuration enthusiast
#

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
#    shopt -s extdebug

    out=`declare -F "$@"`

    if [[ -z "$out" ]]; then

        if [[ $(alias $1) ]]; then

            # TODO: could try the fallback strategy of grepping config files for
            # the name of this alleged alias.

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

    # recenter window
    ( emacsclient -e '(recenter-top-bottom)' ) >&/dev/null

    # Turn off extended shell debugging (should check if it was enabled before
    # in which case, we don't want to disable)
#    shopt -u extdebug
}
