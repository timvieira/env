#!/usr/bin/env python
"""
Find completions of a query given prefix, based on information gain. Assumes a
file of possible completions is given.

Runs in two modes

(1) Testing

    $ python hist-complete.py $COMP_NOTES 'vision '


(2) Completion

    Requires the following bash configuration. Note, there are two things to
    fill out <SCRIPTNAME>, the name of your script, and <COMPLETION_FILE>, a
    file with lines corresponding to completions (i.e. the things we are trying
    to select from, executing this script will essentially just narrow down the
    lines of this file).

    <bashrc>
    _complete_<SCRIPTNAME>()
    {
        COMPREPLY=( $( \
            COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
            COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
            hist-complete.py <COMPLETION_FILE> ) )
    }
    complete -F _complete_<SCRIPTNAME> <SCRIPTNAME>
    </bashrc>


    EXAMPLE
    =======

    Here's an example function fv, which recursively searches the current
    directory for a file to open in the editor.

    function fv {
       find |filter.py $@ --on-unique "$EDITOR {match}"
    }

    We going to define complete behaviour which complements the filter script
    and will help use refine our query until there is a unique file so we can
    open it in the editor.

    <bashrc>
    _complete_fv()
    {
        COMPLETION_FILE=/tmp/find-completions

        find > $COMPLETION_FILE

        COMPREPLY=( $( \
            COMP_LINE=$COMP_LINE  COMP_POINT=$COMP_POINT \
            COMP_WORDS="${COMP_WORDS[*]}"  COMP_CWORD=$COMP_CWORD \
            hist-complete.py $COMPLETION_FILE ) )
    }
    complete -F _complete_fv fv
    </bashrc>


TODO: The bash configuration stuff seems like excessive boilerplate.
"""
from __future__ import print_function
import re, sys
from os import environ, path
sys.path.append('/home/timv/projects')
from env.bin.filter import run #, words
from collections import defaultdict


def camel_space(x):
    """
    Insert spaces implied by camel case.

    >>> camel_space('McDonald')
    'Mc Donald'

    >>> camel_space('thisIsAWordInCamelCaseWord')
    'this Is A Word In Camel Case Word'

    >>> camel_space('howAboutNumbersLike2Or3')
    'how About Numbers Like2 Or3'

    """
    return re.sub(r'((?<=[a-z])[A-Z]|(?<!\A)[A-Z](?=[a-z]))', r' \1', x)


def words(x):
    """
    Extract words.

    >>> words('/path/to/some-file.txt')
    ['path', 'to', 'some', 'file', 'txt']

    >>> words('McDonald')
    ['Mc', 'Donald']

    >>> words('thisIsAWordInCamelCase')
    ['this', 'Is', 'A', 'Word', 'In', 'Camel', 'Case']

    """
    x = camel_space(x)
    return re.findall(r'\w+', x)


# faster to import this way so that we don't drag lots of other math utils along.
sys.path.append('/home/timv/projects/arsenal/arsenal/maths')
from featureselection import kl_filter


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

    lines = [line.strip() for line in open(filename)]

    # ensures prefix of query matches
    matches = list(run(cwords, lines, color=False))

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
        if s > 0.1 or m == 1 or (n == 1 and len(w) > len(currword) > 0):  # strict prefix has been typed
            if testing:
                print('(%g) %s' % (s, w))
                for _, d in ds:
                    print('   %s' % d)
            else:
                print(w)
        else:
            if testing:
                print('## (%g) %s' % (s, w))
                for _, d in ds:
                    print('##   %s' % d)


if __name__ == '__main__':
    complete(*sys.argv[1:])
