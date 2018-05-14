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



# TODO: clever things to keep these files up-to-date?
COMP_ENV=~/.completions/env
COMP_NOTES=~/projects/notes/.index/files
COMP_PROJECTS=~/.completions/projects

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

#function find-repos {
#   find $1 -name '.hg' -type d | sed 's/\\/\.hg$//g'
#   find $1 -name '.git' -type d | sed 's/\\/\.git$//g'
#}

function find-repos {
    locate '*/.hg' | sed 's/\/\.hg$//g'
    locate '*/.git' | sed 's/\/\.git$//g'
}

function update {
    sb
    yellow "Updating completions"

    # global indexing
    yellow "# updatedb (requires sudo)"
    sudo updatedb

    # notes
    yellow "# n2 files"
    n2 --update
    n2 --files

    # environment
    yellow "# env files"
    find $ENV |ignore-filter |grep -v README |grep -v 'site-lisp' > $COMP_ENV
    ls -x $ENV/emacs/*.el >> $COMP_ENV

    # projects
    yellow "# projects files"
    list-projects > $COMP_PROJECTS

    yellow "# done"
}

# TODO: use .projectsrc
function list-projects {
    presentations=`find $PROJECTS/presentations -maxdepth 2 -type d |grep -v '\(\.hg\|\.git\)' |ignore-filter`
    courses=`find $PROJECTS/shelf/courses -type d`
    vcroots=`find-repos`
    #notes=`find $PROJECTS/notes -type d`

    # courses, presentations, repos
    matches="$presentations
$vcroots
$courses
"

    echo "/home/timv/Dropbox/todo"
    echo "$matches" |ignore-filter \
        |grep -v '\.skid'          \
        |grep -v pelican           \
        |grep -v third-party       \
        |grep -v incoming          \
        |grep -v bdslss/reviews    \
        |grep -v '/data/'          \
        |grep -v hw7-xfst/xfsm_api \
        |grep -v '~$'              \
        |bymtime -t
}


#function unique-lines {
#    linepy 'z=set()' 'z.add(line)' 'print "\n".join(sorted(z))'
#}

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
