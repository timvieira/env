#!/usr/bin/env python
"""
To be used in conjunction with minimal bash configuration.

TODO: this seems like a lot of boilerplate...

<goes in your bashrc>

_histcomplete()
{
    COMPREPLY=( $( \
        COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
        COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
        OPTPARSE_AUTO_COMPLETE=1 hist-complete.py <YOUR SCRIPT> ) )
}

complete -F _histcomplete <YOUR SCRIPT>

</goes in your bashrc>
"""

import re, sys
from os import environ, path
from env.bin.filter import main

def complete(prefix, filename='~/.bash_history', freq=3):
    freq = int(freq)
    filename = path.expanduser(filename)

    cwords = environ.get('COMP_WORDS', '').split()[1:] + ['']   # ignore program name, pad by one word
    cword = int(environ.get('COMP_CWORD', 0)) - 1

    currword = cwords[cword]

    lines = [line.strip() for line in file(filename)]

    matches = list(main(cwords, lines, color=False))

    # get words of matches
    possible = [w for line in matches for w in re.findall('\w+', line)]

    # filter words of matches by prefix match
    possible = {x for x in possible if x.startswith(currword) and len(x) >= len(currword)}

    # TODO: remove words common to all hits since there is no information gain

    print ' '.join(possible).encode('utf8')


if __name__ == '__main__':
    from sys import argv
    complete(*argv[1:])
