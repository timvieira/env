#!/usr/bin/env python

"""
TODO: extend to key-value data: filter on key print values.

TODO: Can't distinguish.

  $ fv jhu nlp Grammar.java
  ./src/main/java/edu/jhu/nlp/parsing/grammar/Grammar.java
  ./src/main/java/edu/jhu/nlp/parsing/grammar/ConvertSlavGrammar.java

TODO:

  $ fv ./src/main/java/edu/jhu/nlp/parsing/grammar/Grammar.java
  no results

"""

import re, os, sys

from sys import stdin
from itertools import cycle
from subprocess import Popen, PIPE


colors = red, green, yellow, blue, magenta, cyan = \
    ['\033[3%sm%%s\033[0m' % i for i in xrange(1,7)]


def unique(iterable):
    """ List unique elements, preserving order. Remember all elements ever seen. """
    # unique_everseen('AAAABBBCCDAABBB') --> A B C D
    # unique_everseen('ABBCcAD', str.lower) --> A B C D
    seen = set()
    seen_add = seen.add
    for element in iterable:
        if element not in seen:
            seen_add(element)
            yield element


def filter1(f):
#    f = f.strip()

    # XXX: can't apply filter to nonfiles or nonexistent files (e.g. out-of-date input)
#    if not os.path.exists(f):
#        return True

#    f = f.strip()
#    if f.endswith('.tex'):
#        # filter org-mode tex export files.
#        for line in file(f):
#            if 'Emacs Org-mode version' in line:
#                return False
#    [_, ext] = os.path.splitext(f)
#    ext = ext[1:]  # drop period.
#    if ext not in ['', 'tex', 'org', 'txt', 'rst', 'md', 'markdown',
#                   'py', 'scala', 'java', 'bash', 'el', 'nb', 'ipynb']:
#        return False
#    return True
    return True


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

    >>> words('thisIsAWordInCamelCaseWord')
    ['this', 'Is', 'A', 'Word', 'In', 'Camel', 'Case', 'Word']

    """
    x = camel_space(x)
    return re.findall(r'\w+', x)


def main(filters, lines, color=True):

    # cleanup.
    lines = [l.strip() for l in lines]

    filters = [re.compile(r'\b' + f, re.I) for f in filters]

    for line in unique(lines):

        # XXX: is there a better way to do this? note that highlighting doesn't work.
        # add spaces at camel case word boundaries
        line2 = camel_space(line)

        if not all(f.findall(line) for f in filters) and not all(f.findall(line2) for f in filters):
            continue
        if not filter1(line):   # XXX: not everything is a file, should add an cmdline option
            continue
        if color:
            for f, c in zip(filters, cycle(colors)):
                line = f.sub(lambda m: c % m.group(0), line)
        yield line


if __name__ == '__main__':

    from argparse import ArgumentParser

    # "if (pattern1 and pattern2 ...) then action"

    parser = ArgumentParser(description='Filter a sequence of lines by matching patterns.')
    parser.add_argument('filters', nargs='*', help='filters, each is a regex')

    parser.add_argument('--top', action='store_true', help='call on-unique on the top hit (if there is one).')

    parser.add_argument('--on-unique', help='When a single match is found execute this command.')
    parser.add_argument('--on-fail', help='When a no match is found execute this command.')

    parser.add_argument('-C', '--no-color', dest='color', action='store_false',
                        help='Do not print ANSO color codes.')

    parser.add_argument('-N', dest='msg', action='store_false',
                        help='omit "no results" msg')

    args = parser.parse_args()

    matches = main(filters = args.filters,
                   lines = stdin,
                   color = args.color)
    matches = list(matches)

    if not matches:
        if args.msg:
            print >> sys.stderr, red % 'no results'
        exit(1)

    if args.top:
        print matches[0]
    else:
        for m in matches:
            print m

    if len(matches) == 1 or args.top:
        if args.on_unique:
            Popen(args.on_unique.format(match=re.sub(r'\033\[.*?m', '', matches[0])),
                  stdout=PIPE,
                  stdin=PIPE,
                  stderr=PIPE,
                  shell=True,
                  close_fds=True)

        exit(0)

    elif len(matches) == 0:
        if args.on_fail:
            os.system(args.on_fail)
        exit(1)

    else:
        exit(2)

#    if len(matches) == 1:
#        match = matches[0].strip()
#        if not os.path.isdir(match):
#            os.system('gnome-open %s' % matches[0])
