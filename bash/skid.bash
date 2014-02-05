#
# Skid configuration. Completion and aliases.
#

alias skid-dir='cd `python -c "import skid.config as c; print c.ROOT"`'


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
