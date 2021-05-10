#
# My system for quickly finding/editing notes.
#

source /home/timv/projects/env/bash/colors.bash

function yellow { echo -e "\e[33m$@\e[0m"; }
function light-yellow { echo -e "\e[1;33m$@\e[0m"; }

function remove-color {
    pysed '\033\[.*?m' ''
}

function notes {

    # TODO: search skid and n2 as well?
    # TODO: don't just use filename: include the title (heuristic, first line=title)

    COMP_NOTES=~/projects/notes/.index/files

    colormatches=`cat $COMP_NOTES |bymtime |cut -f2 |~/projects/env/bin/filter.py $@`
    retcode="$?"

    # clickable verion
    #echo "$matches" |linepy 'print("file://" + line)'
    echo "$colormatches"

    export matches=`echo "$colormatches"| remove-color`

    top=`echo "$matches" |head -n1 `

    #n2 $@

    if [[ "$retcode" -eq "0" ]]; then
        # feeling lucky, so we'll open the file for you.

        open-note "$top"

    else

        # Is there a single, general notes.org file in the matches?
#        export generalnote=`echo "$matches"| grep -i /notes.org `
#
#        cnt=`echo "$matches"| grep -i "/notes.org" |wc -l`
#        if [[ "$cnt" -eq "1" ]]; then
#            # only print this message when we didn't have a unique match
#            yellow "Found unique note.org"    # print this on the line of the match.
#            open-note "$generalnote"
#            return
#        fi
#
        topdir=`dirname "$top"`
        cd "$topdir"
        bright_yellow "-> $topdir"

        #yellow "pick a file or be more specific."
    fi
}


function open-note {
    # drop color codes
    match="$1"
    bright_yellow "-> $topdir"
    yellow "opening $match"
    cd `dirname "$match"`

    # dispatch to the appropriate opener; the text editor is the default
    if [[ "$match" =~ .*\.(nb|odp)$ ]]; then
        xdg-open "$match"
    elif [[ "$match" =~ .*\.(ipynb)$ ]]; then
        shutup-and-disown jupyter notebook "$match"
    else
        $EDITOR "$match"
    fi

    #bash   # sigh. Changing directory worked for bash function, but not for
    #       # this script version...
}

# Jump to directory, don't open the file.
function notes-cd {

    COMP_NOTES=~/projects/notes/.index/files

    matches=`cat $COMP_NOTES | xargs dirname |sort |uniq |bymtime |cut -f2 |~/projects/env/bin/filter.py $@`
    retcode="$?"

    # clickable verion
    #echo "$matches" |linepy 'print("file://" + line)'
    echo "$matches"

    #notes.py $@

    if [[ "$retcode" -ne "0" ]]; then
        # We're feeling lucky, so we'll cd into that directory.
        yellow "Multiple hits, taking most recently modified."
    fi

    # Take the top hit
    matches=`echo "$matches" |head -n1`

    # drop color codes
    match=`echo "$matches" |remove-color `

    cd $match

}
