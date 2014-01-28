#!/usr/bin/env python
"""

Very simple indexing files. I like to use it for my notes.

This script extracts a small amount of metadata: title and tags. The title is
taken to be the first line of the file. tags are specific in a comment

(#|%|;) tags: tag1 tag2 tag3

"""

import re, os
from datetime import datetime
from path import path
from whoosh.index import create_in, open_dir
from whoosh.fields import Schema, TEXT, ID, DATETIME
from whoosh.qparser import QueryParser
from whoosh.analysis import KeywordAnalyzer

from arsenal.terminal import cyan, red, yellow, magenta
from arsenal.fsutils import find
from arsenal.iterextras import unique

# globals
DIRECTORY = path('/home/timv/projects/notes/.index')
TRACKED = DIRECTORY / 'files'
NAME = 'index'


def org_export_tex(f):
    "Is `f` a path to an org-mode tex export files?"
    return os.path.exists(f) \
        and f.endswith('.tex') \
        and any(('Emacs Org-mode version' in line) for line in file(f))


def dump_files():
    with file(TRACKED, 'wb') as f:
        for x in unique(find_notes()):
            print >> f, x


def find_notes():
    p = '(.*\.(org|tex)$|.*\\b(TODO|NOTES|LOG)\\b.*)'
    r = ['/home/timv/projects/notes',
         '/home/timv/projects/learn',
         '/home/timv/projects',
         '/home/timv/projects/ldp/write/working/',
         '/home/timv/Dropbox/todo']

    # add python and mathematica scripts notes directory
    for x in find('/home/timv/projects/notes/', regex='.*\.(py|nb|ipynb)$'):
        yield x

    for d in r:
        for x in sorted(find(d, regex=p)):
            if not (not path(x).exists() \
                    or x.endswith('~')
                    or re.findall('(/incoming/|/site-lisp/|/texmf/|/.*~/)', x)
                    or org_export_tex(x)):
                yield x


def files():
    for f in file(TRACKED):
        f = f.strip()
        yield f


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


def _search(q, limit=None):
    q = unicode(q.decode('utf8'))
    ix = open_dir(DIRECTORY, NAME)
    with ix.searcher() as searcher:
        qp = QueryParser('content', schema=ix.schema)
        q = qp.parse(q)
        for hit in searcher.search(q, limit=limit):
            yield hit


def search(*q):
    print
    for hit in _search(' '.join(q)):
        print hit['title']
        print cyan % 'file://%s' % hit['path']
        if hit['tags']:
            print magenta % ' '.join(hit['tags'])
        print


def update():
    "Rebuild index from scratch."

    # create index if it doesn't exist
    if not DIRECTORY.exists():
        create()

    # get handle to Whoosh index
    ix = open_dir(DIRECTORY, NAME)

    with ix.writer() as w, ix.searcher() as searcher:

        fs = set(files())
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

            with file(d) as f:
                content = unicode(f.read().decode('utf8', 'ignore'))


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
    if x is None:
        with file(d) as f:
            x = unicode(f.read().decode('utf8', 'ignore'))

    title = None
    if d.endswith('.tex'):
        title = re.findall(r'\\title\{([\w\W]*?)\}', x)
        if title:
            title = title[0]

    if not title:
        title = x.split('\n')[0]  # use first line of the file as title
        title = re.sub('#\+title:', '', title)

    return title.strip()



def main():
    from argparse import ArgumentParser

    p = ArgumentParser()
    p.add_argument('query', nargs='*')
    p.add_argument('--rebuild', action='store_true',
                   help='rebuild tracked files and index from scratch.')
    p.add_argument('--update', action='store_true')
    p.add_argument('--files', action='store_true')

    args = p.parse_args()

    if args.files:
        dump_files()

    if args.rebuild:
        create()
        dump_files()
        update()

    if args.update:
        update()

    search(*args.query)


if __name__ == '__main__':
    main()
