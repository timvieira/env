"""
Generate a lookup table of unicode characters, their codes, and their names.
"""
import unicodedata
for i in range(2**16):
    x = eval('u"\\u%04x"' % i)
    try:
        print x.encode('utf-8'), '|', "%04x" % ord(x), '|', unicodedata.name(x, '-')
    except IOError:
        continue
