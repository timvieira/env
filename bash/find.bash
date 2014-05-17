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
    find $2 -type f |ignore-filter |grep -i "$1"
}

# fv ("flexible visit" or "find and visit") recursively searches for a file path
# matching specified pattern. Opens the file if a unique match is found.
function fv {
    find -type f | ignore-filter | bymtime - | cut -f2 | grep -v '\.\(pdf\)$' | filter.py $@ --on-unique 'v {match}'
}
