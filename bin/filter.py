#!/usr/bin/env python
import re, os
from terminal import red, green, yellow, blue, magenta, cyan
from sys import argv, stdin, stdout
from itertools import cycle
from os.path import dirname

colors = [red, green, yellow, blue, magenta, cyan]

filters = [re.compile('\\b' + f, re.I) for f in argv[1:]]

matches = [line for line in stdin if all(f.findall(line) for f in filters)]

for line in matches:
    for f, c in zip(filters, cycle(colors)):
        line = f.sub(lambda m: c % m.group(0), line)
    stdout.write(line)

if not matches:
    print red % 'no results'
    exit(1)

if len(matches) == 1:
    os.system('gnome-open %s' % matches[0])
    #os.system('cd %s' % dirname(matches[0]))  # TODO: doesn't change the directory in bash
