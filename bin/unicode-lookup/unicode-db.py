"""
Generate a lookup table of unicode characters, their codes, and their names.
"""
from __future__ import print_function
import unicodedata
for i in range(2**16):
    x = eval('u"\\u%04x"' % i)
    try:
        print(x, '|', "%04x" % ord(x), '|', unicodedata.name(x, '-'), sep=' ')
    except (IOError, UnicodeEncodeError):
        continue
