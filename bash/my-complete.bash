#
# Experimental Bash completion based on information gain
#
# See also bin/hist-complete.py and bin/filter.py

_complete_fv ()
{
    X="/tmp/find-dump"
    find -type f |ignore-filter > $X
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        hist-complete.py $X ) )
}

_complete_e ()
{
    X=$COMP_ENV
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        hist-complete.py $X ) )
}

_complete_notes ()
{
    X=$COMP_NOTES
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        hist-complete.py $X ) )
}

_complete_p ()
{
    X=$COMP_PROJECTS
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        hist-complete.py $X ) )
}

_complete_t ()
{
    X="/tmp/comp-t"
    find ~/Dropbox/todo -type f |grep -v '\.org_archive$' |ignore-filter > $X
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        hist-complete.py $X ) )
}

_complete_vpy ()
{
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        comp-module.py "${COMP_WORDS[$COMP_CWORD]}" ) )
}



# TODO: are there any clever things we can do to speed this up and keep things
# up-to-date?
COMP_ENV="/tmp/comp-e"
COMP_NOTES="/tmp/complete-notes"
COMP_PROJECTS=/tmp/comp-projects
function update {

    # notes files
    find-note-files ~/projects > $COMP_NOTES

    # environment files
    find $ENV |ignore-filter |grep -v 'site-lisp' > $COMP_ENV
    ls -x $ENV/emacs/*.el >> $COMP_ENV

    # project directories
    projname=$(echo $PROJECTS/*/working $PROJECTS/*/*/working $PROJECTS/* |sed 's/ /\n/g')

    # courses
    courses=`find $PROJECTS/courses -type d`

    # version controlled project roots -- be sure to strip off hg directories or
    # else they'll get filtered out
    vcroots=`find $PROJECTS -name '.hg' -type d | grep -v incoming | grep -v '/projects/notes/' |sed 's/\\/\.hg$//g'`

    # sort vc roots so that prefixes come first
    vcroots=`echo "$vcroots" |sort`

    # everything else
    #everythingelse=`find $PROJECTS -type d`

    matches="$projname
$courses
$vcroots
$everythingelse"

    echo "$matches"| ignore-filter| grep -v '/data/' > $COMP_PROJECTS
}



# TODO: check the age of these files before automatically updating
#update

# TODO: create a version of hist complete which uses dir-history.


#  # optcomplete harness for bash shell. You then need to tell
#  # bash to invoke this shell function with a command like
#  # this::
#  #
#  #   complete -F _optcomplete <program>
#  #
#  _optcomplete {
#      COMPREPLY=( $( \
#          COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
#          COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
#          $1 ) )
#  }
#  complete -F _optcomplete skid

complete -F _complete_fv    fv
complete -F _complete_e     e
complete -F _complete_notes notes
complete -F _complete_p     p
complete -F _complete_t     t
complete -F _complete_vpy   vpy
