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

    # TODO: in order to get some shortcircuiting/lazy evaluation, break the
    # search process up by project sources {courses, vcroots, everythingelse}
    # rather than accumulating the lists up front as we do here, make separate
    # calls to a utility which tries to find a match on success goes there -
    # short-circuiting the search process.

    # TODO: cache the output of these functions try searching the cache first -
    # whenever we find something which is "broken" or "missing" we can
    # regenerate it.

    # TODO: add directories of recently modified files to the list of things we
    # search

    # TODO: utility which searches recent files from the command-line (such a
    # tool must already exist!)

    matches=`cat $COMP_PROJECTS`

    echo "$matches" |filter.py $@

    matches=`echo "$matches" |filter.py -C $@`

    # TODO: if all matches have a common (nontrivial) directory go to it
    # instead?

    # TODO: If a directory name is an exact match go to it.
    
    # TODO: repos with more overlap with name should come first
    # e.g.
    #     $ p pdfhacks
    #
    # should prefer ~/projects/pdfhacks over
    #  ~/projects/pdfhacks/bibtex-as-annotation, but doesn't at the moment
    #  simply because of directory ordering.

    for d in $matches; do
        # might want to iterate thru this set..
        cd "$d"
        return
    done
    red "failed to find match for project $1"
}
