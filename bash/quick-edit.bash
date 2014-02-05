#
# Utilities for the compulsive bash configuration enthusiast.
#
# TODO: (low priority) bash completion
# TODO: (low priority) fall-back for aliases?

alias source-bashrc='source ~/.bashrc'
alias sb='source-bashrc'
alias es='edit-script'

# turn on bash's extended debugging options
shopt -s extdebug

function edit-script {
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

# Edit file defining some bash function; we'll even jump to the line number.
function edit-bash-function {

    out=`declare -F "$@"`

    if [[ -z "$out" ]]; then

        if [[ $(alias $1) ]]; then

            # TODO: could try the fallback strategy of grepping config files for
            # the name of this alleged alias.

            echo "Sorry, aliases are not indexed."
            echo `alias $1`
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

}
