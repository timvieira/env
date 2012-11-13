#!/usr/bin/env python
import re, sys
from sys import argv
from os import environ
from collections import Counter
from subprocess import Popen, PIPE

def complete():
    cwords = environ['COMP_WORDS'].split()
    cline = environ['COMP_LINE']
    cpoint = int(environ['COMP_POINT'])
    cword = int(environ['COMP_CWORD'])

    if cword >= len(cwords):
        currword = None
    else:
        currword = cwords[cword]

    c = Counter(w for line in file('/home/timv/.bash_history') if line.startswith('notes') for w in re.findall('\w+', line))

    possible = [k for (k, v) in c.iteritems() if v > 3]

    if currword:
        possible = [x for x in possible if x.startswith(currword) and len(x) >= len(currword)]

    print ' '.join(possible).encode('utf8')


def find_notes():

    p = Popen("""
    find ~/projects -type f -name 'TODO*' -o -name 'NOTE*' -o -name 'LOG*' -o -name "*.tex" -o -name "*.org" \
      |grep -v '~\|#'  \
      |grep -v '/export/' \
      |grep -v 'site-lisp' \
      |grep -iv '\.\(pdf\|log\)$'  # lets assume we want to edit the notes, not view
    """, stdout=PIPE, stderr=PIPE, shell=True)

    (out, err) = p.communicate()

    return out.split()


if 'COMP_WORDS' in environ:
    complete()
    exit(1)


import filter

for x in filter.main(filters = argv[1:], lines = find_notes()):
    print x
