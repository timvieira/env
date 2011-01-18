#!/usr/bin/env python
"""
Clean-up text file:
 - remove nasty control characters
 - remove xml-entities
     + maybe we should replace them when a suitable replacement exists, e.g. "&mdash;" -> "---"
"""

import re
import sys

NASTYCHARS = re.compile('[^\x20-\x7E\s]')
XMLENTS = re.compile('&[a-zA-Z]+;')

try:
    f = file(sys.argv[1], 'r')
except IndexError:
    f = sys.stdin

try:
    o = file(sys.argv[2], 'wb')
except IndexError:
    o = sys.stdout

for line in f:
    line = NASTYCHARS.sub('', line)
    #line = XMLENTS.sub(' ', line)      # TODO: should have an option to keep or to convert to ascii
    #line = line.replace('&', '&amp;')
    o.write(line)
