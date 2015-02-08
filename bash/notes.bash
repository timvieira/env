#
# My system for quickly finding/editing notes.
#

function yellow { echo -e "\e[33m$@\e[0m"; }

function notes {

    # TODO: search skid as well.
    # TODO: don't just use filename. include the title (heuristic, first line=title)

    COMP_NOTES=/home/timv/projects/notes/.index/files

    matches=`cat $COMP_NOTES |bymtime - |cut -f2 |/home/timv/projects/env/bin/filter.py $@`
    retcode="$?"

    # clickable verion
    #echo "$matches" |linepy 'print "file://" + line'
    echo "$matches"

    #notes.py $@

    if [[ "$retcode" -eq "0" ]]; then
        # feeling lucky, so we'll open the file for you.

        # drop color codes
        match=`echo "$matches" |pysed '\\033\[.*?m' '' `

        cd `dirname $match`

        # dispatch to the appropriate opener; the text editor is the default
        if [[ "$match" =~ .*\.(nb|odp)$ ]]; then
            gnome-open $match

        elif [[ "$match" =~ .*\.(ipynb)$ ]]; then
            ipython notebook --pylab inline $match
        else
            $EDITOR "$match"
        fi

        #bash   # sigh. Changing directory worked for bash function, but not for
        #       # this script version...

    else
        yellow "pick a file or be more specific."
    fi
}
