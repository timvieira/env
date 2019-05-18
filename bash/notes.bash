#
# My system for quickly finding/editing notes.
#

function yellow { echo -e "\e[33m$@\e[0m"; }

function notes {

    # TODO: search skid as well.
    # TODO: don't just use filename. include the title (heuristic, first line=title)

    COMP_NOTES=~/projects/notes/.index/files

    matches=`cat $COMP_NOTES |grep -v '\.skid' |bymtime |cut -f2 |~/projects/env/bin/filter.py $@`
    retcode="$?"

    top=`echo "$matches" |head -n1 |pysed '\\033\[.*?m' '' `
    topdir=`dirname "$top"`
    cd "$topdir"
    bright_yellow "-> $topdir"

    # clickable verion
    #echo "$matches" |linepy 'print("file://" + line)'
    echo "$matches"

    #n2 $@

    if [[ "$retcode" -eq "0" ]]; then
        # feeling lucky, so we'll open the file for you.

        # drop color codes
        match=`echo "$matches" |pysed '\\033\[.*?m' '' `

        cd `dirname $match`

        # dispatch to the appropriate opener; the text editor is the default
        if [[ "$match" =~ .*\.(nb|odp)$ ]]; then
            xdg-open "$match"

        elif [[ "$match" =~ .*\.(ipynb)$ ]]; then
            nbopen "$match"
        else
            $EDITOR "$match"
        fi

        #bash   # sigh. Changing directory worked for bash function, but not for
        #       # this script version...

#    else
#        yellow "pick a file or be more specific."
    fi
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
    match=`echo "$matches" |pysed '\\033\[.*?m' '' `

    cd $match

}
