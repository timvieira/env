#!/usr/bin/env python
import re, os
from terminal import red, green, yellow, blue, magenta, cyan
from sys import argv, stdin, stdout
from itertools import cycle
from os.path import dirname

colors = [red, green, yellow, blue, magenta, cyan]

filters = [re.compile('\\b' + f, re.I) for f in argv[1:]]

matches = [line for line in stdin if all(f.findall(line) for f in filters)]

def filter1(f):
    f = f.strip()
    if f.endswith('.tex'):
        # filter org-mode tex export files.
        for line in file(f):
            if 'Emacs Org-mode version' in line:
                return False
    [_, ext] = os.path.splitext(f)
    if ext not in ['', '.tex', '.org', '.txt', '.rst', '.md', '.markdown', 
                   '.py', '.scala', '.java']:
        return False
    return True

matches = filter(filter1, matches)

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
