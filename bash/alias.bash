# ls aliases
alias ll='ls -lAh'
alias la='ls -A'
alias l='ls -CF'
alias lll='ls -h -l --group-directories-first --ignore=*.pyc --ignore=*.o --ignore=*.class' # --ignore-backup
alias lt='ls -lAt'

# cd aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias dl='cd ~/Downloads'
alias de='cd ~/Desktop'

# Colorization aliases
# enable color support of several utils
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias less='less -R'
    alias ack='ack --color --group'
    alias tree='tree -C'
fi

alias tree='tree -I "*.pyc"'
alias less='less -RSimw'

# recursively list files sorted by biggest first.
alias find-big-files="find . -type f -exec ls -s {} \; | sort -n -r"

# emacs with out any of my configuration
alias emacs-plain='shutup-and-disown emacs --no-init-file --no-splash'

# fire-up a webserver for cwd and open it in the browser
alias serve='o http://localhost:8000 && python -m SimpleHTTPServer'

# remove empty lines
alias remove-empty-lines='grep -v "^\s*$"'

# convert spaces to newlines
alias space2newline="sed 's/ /\n/g'"

# order lines by frequency (most frequent first).
alias freq='sort | uniq -c |sort -nr'

# remove ansi escapes
alias nocolor="python -c 'import sys, re; [sys.stdout.write(re.sub(\"\033\[[0-9;]*m\",\"\",x)) for x in sys.stdin]'"

# "alert" using system notifier. Useful for long running commands.
# For example:
#     $ sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias myopen='shutup-and-disown xdg-open'

alias opdf-mapl='myopen /home/timv/projects/presentations/papers/2017-mapl-dyna/mapl.pdf'
alias opdf-nwf='myopen /home/timv/Desktop/read/dyna/nwf-thesis.pdf'
alias opdf-nwf-pseudo='myopen /home/timv/Desktop/read/dyna/nwf-thesis-eb2.pdf'
alias opdf-dyna='myopen /home/timv/.skid/marks/datalog20-paper.pdf'
alias opdf-latex-symbols='myopen /home/timv/.skid/marks/LaTexSymbols-letter.pdf'
alias opdf-unicode-latex-math-symbols='myopen /home/timv/.skid/marks/unimathsymbols.pdf'


function sup {
    yellow "`dir-history |tail`"
    echo
    find | ignore-filter | bymtime | head #tail
    echo
    purple "`((hg log |head) || (git log |head))`"
    echo
    hg st || git status
}
