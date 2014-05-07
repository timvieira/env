#
# Experimental Bash completion based on information gain
#
# See also bin/hist-complete.py and bin/filter.py
#

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

#_complete_es ()
#{
#    arg=`python -c "print '$COMP_LINE'[2:]"`
#    COMPREPLY=$(compgen -c |grep $arg |sort |uniq)
#}

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



# TODO: clever things to keep things up-to-date?
COMP_ENV="/home/timv/.completions/env"
COMP_NOTES='/home/timv/projects/notes/.index/files'
COMP_PROJECTS="/home/timv/.completions/projects"

function ensuredirs {
  for x in `echo $@`; do
      d=`dirname $x`
      if [ ! -f $d ]; then
          mkdir -p $d
      fi
  done
}

ensuredirs \
    $COMP_ENV \
    $COMP_NOTES \
    $COMP_PROJECTS

function update {

    yellow "Updating completions"

    # notes files
    notes.py --files

    # environment files
    find $ENV |ignore-filter |grep -v README |grep -v 'site-lisp' > $COMP_ENV
    ls -x $ENV/emacs/*.el >> $COMP_ENV

    # project directories
    projname=$( echo \
        $PROJECTS/*/working \
        $PROJECTS/*/*/working \
        $PROJECTS/* \
        $PROJECTS/self/* \
        |sed 's/ /\n/g')

    # courses
    courses=`find $PROJECTS/courses -type d`

    # version controlled project roots -- be sure to strip off hg directories or
    # else they'll get filtered out
    vcroots=`find $PROJECTS -name '.hg' -type d | grep -v incoming | sed 's/\\/\.hg$//g'`
    #locate "*/.hg"
    #locate "*/.git"

    # TODO: why did I have this filter "grep -v '/projects/notes/'" above?

    # sort vc roots so that prefixes come first
    vcroots=`echo "$vcroots" |sort`

    # everything else
    #everythingelse=`find $PROJECTS -type d`

    matches="$projname
$courses
$vcroots
$everythingelse"

    echo "/home/timv/Dropbox/todo" > $COMP_PROJECTS
    echo "$matches" |ignore-filter \
        |grep -v bdslss/reviews    \
        |grep -v '/data/'          \
        |grep -v hw7-xfst/xfsm_api \
        |grep -v '~$'              \
        >> $COMP_PROJECTS
}


# TODO: create a version of hist complete which uses dir-history.


# optcomplete harness for bash shell. You then need to tell
# bash to invoke this shell function with a command like
# this::
#
#   complete -F _optcomplete <program>
#
function _optcomplete {
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        $1 ) )
}
complete -F _optcomplete skid

complete -F _complete_fv    fv
complete -F _complete_e     e
complete -F _complete_notes notes
complete -F _complete_p     p
complete -F _complete_t     t
#complete -F _complete_vpy   vpy
#complete -F _complete_es    es
