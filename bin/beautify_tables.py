#!/usr/bin/env python

import sys
from itertools import imap, izip

try:
    input_file = file(sys.argv[1], 'r')
except (IndexError, OSError):
    input_file = sys.stdin

def read_block():
    block = []
    for line in input_file.xreadlines():
        x = line.strip().split()
        if len(x) == 0:
            if len(block) == 0:
                continue
            else:
                break
        block.append(x)
    return block

block = ['dummy value']
while len(block):
    block   = read_block()
    widths  = [max(imap(len, c)) for c in izip(*block)]
    pattern = '   '.join(imap('%{0}s'.format, widths))
    try:
        for row in block:
            print pattern % tuple(row)
        print
        print
    except TypeError: # idk why they picked this to be the exception class
        print 'THIS BLOCK IS JAGGED'
        print block
        raise
