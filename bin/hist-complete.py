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

Testing mode:

   python hist-complete.py $COMP_NOTES 'vision '

"""

import sys
from os import environ, path
from env.bin.filter import main, words
from collections import defaultdict
from scientific.featureselection import kl_filter


def complete(filename, testing=''):
    filename = path.expanduser(filename)

    if testing:
        cwords = testing.split()
        if testing.endswith(' '):
            cwords.append('')
        currword = cwords[-1]

    else:
        cwords = environ.get('COMP_WORDS', '').split()[1:] + ['']   # ignore program name, pad by one word
        currword = cwords[int(environ.get('COMP_CWORD', 0)) - 1]

    lines = [line.strip() for line in file(filename)]

    # ensures prefix of query matches
    matches = list(main(cwords, lines, color=False))

    if not matches:
        return

    d = defaultdict(list)
    for line in set(matches):
        for w in words(line.lower()):
            if w.startswith(currword):
                d[line].append(w)

    kl = kl_filter(d.items(), verbose=False)
    kl = list(kl)

    n = len(set(matches))
    m = len(kl)

    for s, w, ds in kl:
        # when we've narrowed it down to one line or word given the current
        # prefix, KL divergence will be zero.
        if s > 0.01 or m == 1 or (n == 1 and len(w) > len(currword) > 0):  # strict prefix has been typed
            if testing:
                print '(%g) %s' % (s, w)
                for _, d in ds:
                    print '   %s' % d
            else:
                print w.encode('utf8')
        else:
            if testing:
                print '## (%g) %s' % (s, w)
                for _, d in ds:
                    print '##   %s' % d

if __name__ == '__main__':
    from sys import argv
    complete(*argv[1:])
