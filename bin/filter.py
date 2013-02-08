#!/usr/bin/env python
import re, os, sys
from terminal import red, green, yellow, blue, magenta, cyan
from sys import stdin, stdout
from itertools import cycle
from subprocess import Popen, PIPE

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

        # XXX: is there a better way to do this? note that highlighting doesn't work.
        # add spaces at camel case word boundaries
        line2 = re.sub(r'((?<=[a-z])[A-Z]|(?<!\A)[A-Z](?=[a-z]))', r' \1', line.strip())

        if not all(f.findall(line) for f in filters) and not all(f.findall(line2) for f in filters):
            continue
        if not filter1(line):
            continue
        for f, c in zip(filters, cycle(colors)):
            line = f.sub(lambda m: c % m.group(0), line)
        yield line


if __name__ == '__main__':

    from argparse import ArgumentParser

    # "if (pattern1 and pattern2 ...) then action"

    parser = ArgumentParser(description='Filter a sequence of lines by matching patterns.')
    parser.add_argument('filters', nargs='+', help='filters, each is a regex')

    parser.add_argument('--on-unique', help='When a single match is found execute this command.')
    parser.add_argument('--on-fail', help='When a no match is found execute this command.')

    parser.add_argument('-C', '--no-color', dest='color', action='store_false',
                        help='Do not print ANSO color codes.')

    args = parser.parse_args()

    matches = main(filters = args.filters,
                   lines = stdin,
                   color = args.color)
    matches = list(matches)

    if not matches:
        print >> sys.stderr, red % 'no results'
        exit(1)

    for m in matches:
        stdout.write(m)

    if len(matches) == 1:
        if args.on_unique:
            Popen(args.on_unique.format(match=re.sub('\\033\[.*?m', '', matches[0])),
                  stdout=PIPE,
                  stdin=PIPE,
                  stderr=PIPE,
                  shell=True, 
                  close_fds=True)

        exit(0)

    elif len(matches) == 0:
        if args.on_fail:
            os.system(args.on_fail)
        exit(1)

    else:
        exit(2)

#    if len(matches) == 1:
#        match = matches[0].strip()
#        if not os.path.isdir(match):
#            os.system('gnome-open %s' % matches[0])
