#
# Shortcuts for finding and editing stuff if those annoying deep directories
# (e.g., Java projects).
#
# Requirements:
#
#   ignore-filter must be defined
#
#   must have filter.py

# grep file path from recursive directory listing
function f {
    find -type f |ignore-filter | filter.py $@
}

# fv ("flexible visit" or "find and visit") recursively searches for a file path
# matching specified pattern. Opens the file if a unique match is found.
function fv {
    find -type f | ignore-filter |grep -v 'scrap\|junk\|old' | bymtime | cut -f2 | grep -v '\.\(pdf\|dir\|bak\|dat\)$' | filter.py $@ --on-unique 'v {match}'
}

function ov {
    find -type f | ignore-filter |grep -v 'scrap\|junk\|old' | bymtime | cut -f2 | grep -v '\.\(pdf\|dir\|bak\|dat\)$' | filter.py $@ --on-unique 'xdg-open {match}'
}
