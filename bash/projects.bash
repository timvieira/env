#
# Jump to project directory by name (with flexible matching on path).
#
function p {
    # calling with no arguments lands you in the projects directory.
    if [[ "$#" -eq 0 ]]; then
        cd $PROJECTS
        yellow "Recent projects"
        cat $COMP_PROJECTS | bymtime | head -n 10
        return
    fi

    # TODO: add directories of recently modified files to the list of things we search

    # TODO: utility which searches recent files from the command-line (such a
    # tool must already exist!)

    # TODO: if all matches have a common (nontrivial) directory go to it instead?

    # TODO: If a directory name is an exact match go to it. Especially, if one
    # of the folders in ~/projects is an exact match.

    # TODO: repos with more overlap with name should come first e.g. `$ p pdfhacks`

    matches=`cat $COMP_PROJECTS |bymtime -t`
    echo "$matches" |filter.py $@

    # run same search, but with color disabled.
    matches=`echo "$matches" |filter.py -C $@`
    for d in $matches; do
        # might want to iterate thru this set..
        cd "$d"
        return
    done
    red "failed to find match for project $1"
}
