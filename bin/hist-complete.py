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

import re, os, sys
from os import environ, path
from collections import Counter
from arsenal.terminal import red
from env.bin.filter import main

def complete(prefix, filename='~/.bash_history', freq=3):
    freq = int(freq)
    filename = path.expanduser(filename)

    cwords = environ.get('COMP_WORDS', '').split()[1:]   # ignore program name.
    #cline = environ['COMP_LINE']
    #cpoint = int(environ['COMP_POINT'])
    cword = int(environ.get('COMP_CWORD', 0)) - 1

    if cword >= len(cwords):
        currword = None
    else:
        currword = cwords[cword]

    lines = [line.strip() for line in file(filename)]
#    ix = {w: line for line in lines for w in re.findall('\w+', line)}
#    c = Counter(ix.keys())
#    possible = [k for (k, v) in c.iteritems() if v >= freq]

    matches = list(main(cwords, lines, color=False))

#    print >> sys.stderr, '===='
#    print >> sys.stderr, '\n'.join(cwords)
#    print >> sys.stderr, '===='
#    print >> sys.stderr, '\n'.join(matches)
#    print >> sys.stderr, '===='
#    print >> sys.stderr, '\n'.join(lines)

#    print >> sys.stderr, '\n'.join(cwords)

    if not currword:
        print ''
        return

    # get words of matches
    possible = [w for line in matches for w in re.findall('\w+', line)]

#    print >> sys.stderr, '==='
#    print >> sys.stderr, ' '.join(possible)

    # filter words of matches by prefix match
    possible = {x for x in possible if x.startswith(currword) and len(x) >= len(currword)}

    print ' '.join(possible).encode('utf8')


if __name__ == '__main__':
    from sys import argv
    complete(*argv[1:])
