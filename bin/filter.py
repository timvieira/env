#!/usr/bin/env python

"""Filter stdin line-by-line based on keyword filters.

- TODO: "smart case" filter --- if query includes caps -> case sensitive,
  insensitive, otherwise.

- TODO: Can't distinguish.

  $ fv jhu nlp Grammar.java
  ./src/main/java/edu/jhu/nlp/parsing/grammar/Grammar.java
  ./src/main/java/edu/jhu/nlp/parsing/grammar/ConvertSlavGrammar.java

  - [2018-05-15 Tue] Related annoyance

    $ p scea
    /home/timv/projects/presentations/papers/2018-scea
    /home/timv/projects/scea

    The issue: I can refine the query to hit the first one "scea papers", but
    that latter requires "projects/scea" (with the slash), which is a annoying
    to think of and to type. The recency heuristic just means the result is a
    unpredictable as I'm currently swapping between these two projects
    constantly.

- TODO: underscore/lackof is a problem.

  Neither 'two_line_search' or 'twolinesearch' match the query 'two line search'

"""
import re, os, sys

from sys import stdin
from itertools import cycle


colors = red, green, yellow, blue, magenta, cyan = \
    ['\033[3%sm%%s\033[0m' % i for i in range(1,7)]


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


# Note: unused in completion script
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


# TODO: create a submodule with common filters so that we can use this in
# notes.py
def filter2(f):
    "Content-based file filters."
    # TODO: add to where ever org-mode export files are filtered
    if os.path.exists(f):
        if f.endswith('.c') or f.endswith('.cpp'):
            if '/* Generated by Cython' in open(f, encoding='utf-8', errors='ignore').read(1024):
                return False
            if '#error Do not use this file' in open(f, encoding='utf-8', errors='ignore').read(1024):
                return False
        if f.endswith('.html'):
            if '<!-- Generated by Cython' in open(f, encoding='utf-8', errors='ignore').read(1024):
                return False
        if f.endswith('.tex'):
            if 'Emacs Org-mode version' in open(f, encoding='utf-8', errors='ignore').read(1024):
                return False
        if f.endswith('.pdf'):
            if 'Emacs Org-mode version' in open(f, encoding='utf-8', errors='ignore').read(128*1024):
                return False
        # ignore latex's junk
        if re.findall('\.(blg|aux|log|bbl|out)$', f): # looks like a tex output
            # check for similarly named tex file
            if os.path.exists(re.sub('\.(blg|aux|log|bbl|out)$', '.tex', f)):
                return False

    return True


def run(filters, lines, color=True):

    # cleanup.
    lines = [l.strip() for l in lines]

    # substring match -> higher recall
    substring_match = 1
    if substring_match:
        filters = [re.compile(re.escape(f), re.I) for f in filters]
    else:
        # XXX: trial period. This version does not use regex filters.
        # new:
        filters = [re.compile(r'\b' + re.escape(f), re.I) for f in filters]
        # old:
        #filters = [re.compile(r'\b' + f, re.I) for f in filters]

    for line in unique(lines):

        if substring_match:
            if not all(f.findall(line) for f in filters):
                continue
        else:
            # camel case expansion not needed for substring match.

            # XXX: is there a better way to do this? note that highlighting doesn't work.
            line2 = camel_space(line)  # add spaces at camel case word boundaries
            if not (all(f.findall(line) for f in filters) \
                    or all(f.findall(line2) for f in filters)):
                continue

        if not filter2(line):
            continue

        if color:
            for f, c in zip(filters, cycle(colors)):
                line = f.sub(lambda m: c % m.group(0), line)

        yield line


def main():
    from argparse import ArgumentParser

    # "if (pattern1 and pattern2 ...) then action"

    parser = ArgumentParser(description='Filter a sequence of lines by matching patterns.')
    parser.add_argument('filters', nargs='*', help='filters, each is a regex')

    parser.add_argument('--top', action='store_true', help='call on-unique on the top hit (if there is one).')

    parser.add_argument('--on-unique', help='When a single match is found execute this command.')
    parser.add_argument('--on-fail', help='When a no match is found execute this command.')

    parser.add_argument('-C', '--no-color', dest='color', action='store_false',
                        help='Do not print ANSI color codes.')

    parser.add_argument('-N', dest='msg', action='store_false',
                        help='omit "no results" msg')

    args = parser.parse_args()

    matches = run(filters = args.filters,
                  lines = stdin,
                  color = args.color)
#    matches = list(sorted(matches, key=len))   # sorter strings go first because they might be prefixes of longer paths.
    matches = list(matches)

    from path import Path
    high = []
    filters = [re.compile(re.escape(f), re.I) for f in args.filters]
    for m in matches:
        b = Path(m).basename()
        if any(f.findall(b) for f in filters) or b.lower().startswith('note'):
            high.append(m)

    if len(high) == 1:
        print(yellow % '-> using high-priority match', file=sys.stderr)
        matches = high

    if not matches:
        if args.msg:
            print(red % 'no results', file=sys.stderr)
        exit(1)

    if args.top:
        print(matches[0])
    else:
        for m in matches:
            print(m)

    if len(matches) == 1 or args.top:
        if args.on_unique:
            from subprocess import Popen, PIPE
            cmd = args.on_unique.format(match=re.sub(r'\033\[.*?m', '', matches[0]))
            Popen(cmd,
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


if __name__ == '__main__':
    main()
