#!/usr/bin/env python
"""
Searching my notes!

This script extracts a small amount of metadata: title and tags. The title is
taken to be the first line of the file. tags are specified in a comment

(#|%|;) tags: tag1 tag2 tag3

TODO:

 * add option for listing recently modified files.

"""

import re, os, codecs
from datetime import datetime
from path import path
from whoosh.index import create_in, open_dir
from whoosh.fields import Schema, TEXT, ID, DATETIME
from whoosh.qparser import QueryParser
from whoosh.analysis import KeywordAnalyzer

from arsenal.terminal import red, cyan, yellow, magenta
from arsenal.fsutils import find
from arsenal.iterextras import unique
from arsenal.humanreadable import datestr


from configparser import ConfigParser
parser = ConfigParser(allow_no_value=True)
parser.optionxform = unicode       # don't lower case keys
parser.read(path('~/.notesrc').expand())

# TODO: config file validation. Sources should be directories or
# files. Include/exclude should be regular expressions. Add option for glob?

DIRECTORY = path(parser.get('config', 'index')).expand()
TRACKED = DIRECTORY / 'files'
NAME = 'index'


def dump_files():
    with file(TRACKED, 'wb') as f:
        for x in unique(find_notes()):
            print >> f, x
    print yellow % 'wrote tracked files to %s' % TRACKED


def find_notes():
    for d, _ in sorted(parser.items('sources')):
        for x in sorted(find(path(d).expand())):
            if not any(re.match(p, x) for p, _ in parser.items('include')):
                continue
            if any(exclude(p, x) for p, _ in parser.items('exclude')):
                continue
            yield x


def exclude(p, x):
    if p.startswith('%'):  # magic filter function
        if magic_filters[p](x):
            return True
    else:
        if re.findall(p, x):
            return True


def org_export_tex(f):
    "Is `f` a path to an org-mode tex export files?"
    return (os.path.exists(f) \
            and f.endswith('.tex') \
            and any(('Emacs Org-mode version' in line) for line in file(f)))


magic_filters = {
    '%file_exists': lambda x: not path(x).exists(),
    '%org_export_tex': org_export_tex,
}


def create():
    """ Create a new Whoosh index.. """
    print 'creating new index in directory %s' % DIRECTORY
    os.system('rm -rf %s' % DIRECTORY)
    os.mkdir(DIRECTORY)
    schema = Schema(path = ID(stored=True, unique=True),
                    title = TEXT(stored=True),
                    content = TEXT(stored=True),
                    tags = TEXT(stored=True, analyzer=KeywordAnalyzer()),
                    mtime = DATETIME(stored=True))
    create_in(DIRECTORY, schema, NAME)


def drop():
    "Drop existing index."
    assert DIRECTORY.exists()
    os.system('rm -rf ' + DIRECTORY)
    print 'dropped index', DIRECTORY


def mtime_if_exists(x):
    p = path(x)
    return datetime.fromtimestamp(p.mtime) if p.exists() else None


def _search(q, limit=None):
    q = unicode(q.decode('utf8'))
    ix = open_dir(DIRECTORY, NAME)
    with ix.searcher() as searcher:

        if not q:  # empty query
            for d in sorted(ix.searcher().documents(),
                            key = lambda x: mtime_if_exists(x['path']),
                            reverse = 1):
                yield d

        else:
            qp = QueryParser('content', schema=ix.schema)
            q = qp.parse(q)
            for hit in searcher.search(q, limit=limit):
                yield hit


def search(*q):
    print
    for hit in _search(' '.join(q)):

        mt = mtime_if_exists(hit['path'])
        if mt is None:
            mt = hit['mtime']
            mtime_msg = 'no longer exists!'
            mtime_msg = red % '(%s)' % mtime_msg
        else:
            mtime_msg = datestr(mt)
            mtime_msg = magenta % '(%s)' % mtime_msg

        print hit['title'].encode('utf-8'), mtime_msg
        print cyan % 'file://%s' % hit['path']
        if hit['tags']:
            print magenta % ' '.join(hit['tags'])
        print


def update():
    "re-index files which have change."

    # create index if it doesn't exist
    if not DIRECTORY.exists():
        create()

    # get handle to Whoosh index
    ix = open_dir(DIRECTORY, NAME)

    with ix.writer() as w, ix.searcher() as searcher:

        fs = set(find_notes())
        fs = filter(os.path.exists, fs)

        for d in sorted(map(path, fs), key=lambda x: x.mtime, reverse=True):

            # lookup document mtime in the index; don't add or extract info if
            # you don't need it.
            result = searcher.find('path', unicode(d))

            mtime = datetime.fromtimestamp(d.mtime)

            if not result:
                print '[INFO] new document:', d

            else:
                assert len(result) == 1, 'should be unique.'
                result = result[0]
                if mtime <= result['mtime']:   # already up to date
                    # Since we've sorted files by mtime, we know that files
                    # after this one are older, and thus we're done.
                    return

                print '[INFO] update:', d

            with codecs.open(d, 'r', encoding='utf8', errors='ignore') as f:
                content = f.read()

            if not content.strip():
                print '[INFO] document empty.'
                continue

            title = extract_title(d, content)

            # a line begining with a comment marker
            tags = re.findall('^[#%:;]\s*tags:\s*(.*)', content, re.MULTILINE)

            if tags:
                tags = [x.strip() for x in tags[0].split()]

            w.update_document(path = unicode(d),
                              title = title,
                              content = content,
                              mtime = mtime,
                              tags = tags)


def extract_title(d, x=None):
    "Apply heuristics for extracting document titles."

    if d.endswith('.ipynb') or d.endswith('.nb'):   # mathematica or jupyter notebook
        return d

    if x is None:
        with codecs.open(d, 'r', encoding='utf8') as f:
            x = f.read()

    title = None
    if d.endswith('.odp'):
        title = d

    elif d.endswith('.tex'):
        title = re.findall(r'\\(?:icml)?title\{([\w\W]*?)\}', x)
        if title:
            title = title[0]

            # remove common cruft from titles

            # * LaTeX line breaks
            title = re.sub(r'\\\\\s+', '', title)

            # * acknowledgment via \thanks{...}
            title = re.sub(r'\\(footnote|thanks)\{.*', '', title)

            #title = re.sub(r'\\[a-zA-Z]', '', title)

            # * curly braces
            #title = re.sub(r'[{}]', '', title)

            # comment symbols
            title = re.sub(r'#|//|/\*', '', title)

            # remove excess whitespace
            title = re.sub(r'\s+', ' ', title)


    if not title:
        lines = x.split('\n')

        # use first (non-shebang) line of the file as title
        title = lines[0]
        for line in lines:
            line = line.strip()

            # remove org-mode markup or python doc string.
            line = re.sub('^(#\+title:|""")', '', line)

            if line:
                if not line.startswith('#!') and not line.startswith('# -*-'): # skip shebang and encoding line
                    title = line
                    break

    title = re.sub('\s\s+', ' ', title)

    return title.strip()



def main():
    from argparse import ArgumentParser

    p = ArgumentParser()
    p.add_argument('query', nargs='*')
    p.add_argument('--rebuild', action='store_true',
                   help='Rebuild indexed from scratch.')
    p.add_argument('--update', action='store_true',
                   help='Search for changes and update index.')
    p.add_argument('--files', action='store_true',
                   help='Cache tracked files.')

    args = p.parse_args()

    if args.files:
        dump_files()
        return

    if args.rebuild:
        create()
        dump_files()
        update()
        return

    if args.update:
        dump_files()
        update()
        return

    search(*args.query)


if __name__ == '__main__':
    main()
