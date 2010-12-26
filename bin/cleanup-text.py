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

for line in sys.stdin:
    line = NASTYCHARS.sub('', line)
    line = XMLENTS.sub(' ', line)
    line = line.replace('&', '&amp;')
    sys.stdout.write(line)
