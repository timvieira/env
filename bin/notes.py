#!/usr/bin/env python
"""Search my notes!

Finds notes living all over your hard drive and build an index over them for
efficient keyword search.

We have some support or fuzzy term matching, which allows for typos in the
queries and documents.

We also support limited metadata indexing and extraction, including title, tags,
and modification time.

Many heuristics for title are implemented depending on the filetype.

(#|%|;) tags: tag1 tag2 tag3

TODO:

 - Add a cleanup option for deleting files which no longer exist (Currently, we
   have to drop and recreate the index in order to do that.)

 - Config file

   - Validation

   - Sources should be directories or files.

   - Glob instead of regex?

   - Other options: add things matching a pattern under a specific directory.

"""

import re, os, codecs
from configparser import ConfigParser
from datetime import datetime
from path import path

from whoosh.analysis import KeywordAnalyzer, STOP_WORDS
from whoosh.index import create_in, open_dir
from whoosh.fields import Schema, TEXT, ID, DATETIME
from whoosh.qparser import MultifieldParser
#from whoosh.query import FuzzyTerm

from arsenal.terminal import red, cyan, yellow, magenta
from arsenal.iterextras import unique, take
from arsenal.humanreadable import datestr
from arsenal.fsutils import files


config = ConfigParser(allow_no_value=True)
config.optionxform = unicode       # don't lower case keys
config.read(path('~/.notesrc').expand())

DIRECTORY = path(config.get('config', 'index')).expand()
TRACKED = DIRECTORY / 'files'
NAME = 'index'


def extract_content(f):
    "What file extensions should we extract content from?"
    return f.ext in {'', '.md', '.py', '.pyx', '.html', '.bib',
                     '.pxd', '.org', '.tex', '.rst'}


re_stopwords = re.compile(r'\b(%s)\b\s*' % '|'.join(STOP_WORDS), re.I)
def remove_stopwords(x):
    """
    >>> remove_stopwords('A man saw the boy with his telescope.')
    'man saw boy his telescope.'
    """
    return re_stopwords.sub('', x)


def dump_files():
    "Write tracked files to index directory."
    with file(TRACKED, 'wb') as f:
        for x in unique(find_notes()):
            print >> f, x
    print yellow % 'wrote tracked files to %s' % TRACKED


def find_notes():
    "Search for notes given the settings in config."
    for d, _ in sorted(config.items('sources')):
        for x in sorted(files(path(d).expand())):
            x = path(x)
            if not any(re.match(p, x) for p, _ in config.items('include')):
                continue
            if any(exclude(p, x) for p, _ in config.items('exclude')):
                continue
            yield x


def exclude(p, x):
    if p.startswith('%'):        # magic filter function
        if magic_filters[p](x):
            return True
    else:
        if re.findall(p, x):
            return True


def org_export_tex(f):
    "Is `f` a path to an org-mode tex export files?"
    # a tex file with an org file next to it.
    return f.endswith('.tex') and path(f[:-3] + 'org').exists()


# catalog of magic filters
magic_filters = {
    '%file_exists': lambda x: not x.exists(),
    '%org_export_tex': org_export_tex,
}


def create():
    "Create a new Whoosh index."
    print yellow % 'creating new index in directory %s' % DIRECTORY
    os.system('rm -rf %s' % DIRECTORY)
    os.mkdir(DIRECTORY)
    schema = Schema(path = ID(stored=True, unique=True),
                    filename = TEXT(stored=True),   # filename store as text too.
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
    "Grab mtime for a file that exists."
    return datetime.fromtimestamp(x.mtime) if x.exists() else None


def ls(limit=None):
    ix = open_dir(DIRECTORY, NAME)
    for d in take(limit, sorted(ix.searcher().documents(),
                                key = lambda x: mtime_if_exists(x['path']),
                                reverse = 1)):
        yield d


def _search(q, limit=None):
    q = unicode(q.decode('utf8'))
    ix = open_dir(DIRECTORY, NAME)
    w = {'title': 4, 'tags': 3, 'path': 2, 'filename': 2, 'content': 1}
    p = MultifieldParser(fieldnames = w,
                         fieldboosts = w,
                         #termclass = FuzzyTerm,
                         schema = ix.schema)
    with ix.searcher() as searcher:
        # Whoosh chokes on queries with stop words, so remove them.
        q = remove_stopwords(q)
        q = p.parse(q)
        hits = list(searcher.search(q, limit=limit))
        for hit in hits:
            yield hit

        for k, s in  searcher.key_terms(docnums=[h.docnum for h in hits],
                                        fieldname='content',
                                        numterms=10):
            print '%.1f %s' % (s, k)


def search(q, **kw):
    hits = _search(q, **kw) if q else ls(**kw)
    print
    for hit in hits:
        mt = mtime_if_exists(path(hit['path']))
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
    "Re-index files which have changed."

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

            content = ''
            if extract_content(d):
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
                              filename = ' '.join(re.split('[/\-\._]', d)),
                              tags = tags)


def extract_title(d, x):
    "Apply heuristics for extracting document titles."

    if d.endswith('.ipynb') or d.endswith('.nb'):   # mathematica or jupyter notebook
        return d

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
            line = re.sub('^(#\+title:|"""|#|\*)', '', line)
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
    p.add_argument('--limit', type=int, default=5)
    p.add_argument('-a', action='store_true')

    args = p.parse_args()

    if args.a:
        args.limit = None

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

    if args.limit:
        print yellow % 'Showing top %s results' % args.limit

    q = ' '.join(args.query)
    search(q, limit=args.limit)


if __name__ == '__main__':
    main()
