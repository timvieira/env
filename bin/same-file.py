#!/usr/bin/env python

"""
Simple way to test if to paths refer to the same file.
"""

from path import path
from sys import argv
from collections import defaultdict

paths = map(path, argv[1:])

d = defaultdict(list)
for x in paths:
    d[x.stat()].append(x)

if len(d) > 1:
    print 'more than one cluster found:'
    for i, x in enumerate(d):
        print '%s: %s' % (i, ' '.join(d[x]))
    exit(1)

exit(0)
