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

import re
from os import environ, path
from collections import Counter

def complete(prefix):

    h = path.expanduser('~/.bash_history')

    cwords = environ['COMP_WORDS'].split()
    #cline = environ['COMP_LINE']
    #cpoint = int(environ['COMP_POINT'])
    cword = int(environ['COMP_CWORD'])

    if cword >= len(cwords):
        currword = None
    else:
        currword = cwords[cword]

    c = Counter(w for line in file(h) if line.startswith(prefix) for w in re.findall('\w+', line))

    possible = [k for (k, v) in c.iteritems() if v > 3]

    if currword:
        possible = [x for x in possible if x.startswith(currword) and len(x) >= len(currword)]

    print ' '.join(possible).encode('utf8')


if __name__ == '__main__':
    from sys import argv
    complete(argv[1])
