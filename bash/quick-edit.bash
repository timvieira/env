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

        # TODO: the `ack` search only covers personal config files, should
        # probably extend to other configuration, e.g., stuff in `/etc`.

        #ackresults=$(ack --nogroup "^[^#]*\\b$1\\b" ~/projects/env/bash ~/projects/env/bashrc)
        ackresults=$(ack --nogroup "^[^#]*alias \\b$1\\b" ~/projects/env/bash ~/projects/env/bashrc)

        if [[ $ackresults ]]; then
            green '============================================'
            green 'ack results'
            green '============================================'
            # filters out lines which are commented out with a pound sign
            echo "$ackresults"
            green '============================================'

            # open file at lineno with visit
            visit `echo $ackresults |nocolor |linepy 'print re.findall("[^:]*:[0-9]+", line)[0]; break'`

            # recenter window
            ( emacsclient -e '(recenter-top-bottom)' ) >&/dev/null

        else
            echo 'ack search failed'
        fi


        if [[ $(alias $1) ]]; then

            # TODO: (better strategy below) could try the fallback strategy of
            # grepping config files for the name of this alleged alias.

            # TODO: type -a provides some information about aliases
            #
            #   $ type -a es
            #   es is aliased to `edit-script
            #
            # similar informaiton provided by alias
            #
            #   $ alias es
            #   alias es='edit-script'

            echo "Sorry, aliases are not indexed."
            echo `alias $1`
            return 1
        fi

        echo "failed to find source for '$@'."

        return 1

    else
        echo $out

        # convert output into a bash array
        array=(`echo "$out"`)
        lineno=${array[1]}
        filename=${array[2]}

        # open file at lineno with visit
        visit +$lineno:0 "$filename"

        # recenter window
        ( emacsclient -e '(recenter-top-bottom)' ) >&/dev/null

    fi

}
