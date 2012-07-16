#!/usr/bin/env python
"""
Very basic keyword search.

TODO:
 - show context
 - better ranking
 - consider building a simple inverted index?
"""

import re, sys
from fsutils import find

word_pat = re.compile('\w+')

def bagofwords(s):
    return {w.lower() for w in word_pat.findall(s)}

def matcher(query):

    q = bagofwords(query)

    def kw(filename):
        with file(filename, 'r') as f:
            contents = f.read()
            words = bagofwords(contents)
            return q.issubset(words)

    return kw

def main():
    q = ' '.join(sys.argv[1:])
    for x in find('.', filterfn=matcher(q)):
        print x

if __name__ == '__main__':
    main()
