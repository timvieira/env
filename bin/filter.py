#!/usr/bin/env python
import re, os, sys
from terminal import red, green, yellow, blue, magenta, cyan
from sys import argv, stdin, stdout
from itertools import cycle
from os.path import dirname


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


def main(filters, lines, color=True):
    colors = [red, green, yellow, blue, magenta, cyan] if color else ['%s']
    filters = [re.compile('\\b' + f, re.I) for f in filters]

    for line in lines:
        if not all(f.findall(line) for f in filters):
            continue
        if not filter1(line):
            continue
        for f, c in zip(filters, cycle(colors)):
            line = f.sub(lambda m: c % m.group(0), line)
        yield line


if __name__ == '__main__':

    # TODO: use argparse, b/c this is hideous
    try:
        # TODO: better name; `-c` sounds like "use color" instead of "don't use color"
        sys.argv.remove('-c')
    except ValueError as e:
        color = True
    else:
        color = False

    matches = main(filters = argv[1:],
                   lines = stdin,
                   color = color)
    matches = list(matches)

    if not matches:
        print >> sys.stderr, red % 'no results'
        exit(1)

    for m in matches:
        stdout.write(m)

#    if len(matches) == 1:
#        match = matches[0].strip()
#        if not os.path.isdir(match):
#            os.system('gnome-open %s' % matches[0])

