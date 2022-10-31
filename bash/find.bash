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

function fff {
    find -type f \
     |ignore-filter \
     |grep -v '(/scrap/\|\.log\|\.out\|/out/\|/junk/\|/old/\|/docs/)' \
     |bymtime |cut -f2 |grep -v '\.\(pdf\|dir\|bak\|dat\)$'
}

# fv ("flexible visit" or "find and visit") recursively searches for a file path
# matching specified pattern. Opens the file if a unique match is found.
#function fv { fff | filter.py $@ --on-unique 'v {match}' }

function fv {
    query=$(echo "$@" | sed 's/ /\ /g')
    local results=$(fff | fzf -q "$query" --height 40% --layout reverse --border --preview 'cat {}' --color 'fg:#bbccdd,fg+:#ddeeff,bg:#334455,preview-bg:#223344,border:#778899')
    if [ -z "$results" ]
    then
        echo
    else
        echo "$results"
        v $results
    fi
}


# find and open
function fo {
    fff | filter.py $@ --on-unique 'xdg-open {match}'
}

#
# find pdf
#
function fpdf {
    # calling with no arguments lands you in the projects directory.
    if [[ "$#" -eq 0 ]]; then
        cd $PROJECTS
        yellow "Recent PDFs"
        locate '*.pdf' | bymtime | head -n 10
        return
    fi

    query=$(echo "$@" | sed 's/ /\ /g')
    local results=`locate '*.pdf' |grep /home/timv/ |bymtime -t |fzf -q $query`
    if [ -z "$results" ]
    then
        echo
    else
        echo "$results"
        xdg-open $results
    fi

#    matches=`echo "$matches" |nocolor $@`
#    for m in $matches; do
#        # might want to iterate thru this set..
#        #cd `dirname "$m"`
#        xdg-open "$m"
#        return
#    done
#    red "failed to find match for PDF $1"
}
alias fp='fpdf'

# Utility for looking up emojis - results are automatically placed on clipboard
# like l2u (my latexify script).
function emo {
    # for k in emoji.unicode_codes.EMOJI_ALIAS_UNICODE_ENGLISH:
    #     if 'wink' in k: print(k)
    out=`python -c "from emoji import emojize as emo; print(emo(':$1:', use_aliases=True))" `
    echo "$out" |xsel --clipboard
    xsel --clipboard
}
