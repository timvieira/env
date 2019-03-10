#!/usr/bin/env python
"""Convert STDIN to UTF-8 based on character encoding detection

"""
from __future__ import print_function
import sys, itertools
from chardet.universaldetector import UniversalDetector

detector = UniversalDetector()
lines = []
for line in sys.stdin:
    lines.append(line)
    detector.feed(line)
    if detector.done:
        break
detector.close()

print(detector.result, file=sys.stderr)

encoding = detector.result['encoding']
for line in itertools.chain(lines, sys.stdin):
    converted = line.decode(encoding, 'replace').encode('utf8')
    sys.stdout.write(converted)
