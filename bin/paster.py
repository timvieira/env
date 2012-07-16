#!/usr/bin/env python
"""
Dump contents of clipboard to email message or text document.

TODO:
 - make standalone
 - ship to hanna
 - copy to desktop (needs launcher)
"""

from sys import argv, path

# link to python-extras
path.append('/home/timv/projects/extras/python')

from terminal.clipboard import clipboard_get
from terminal import yellow, red
from humanreadable import str2bool
from sendmail.send_gmail import send_gmail
from misc import attn
from fsutils import find_new_title

if len(argv) != 2:
    print 'Who would you like to send to?'
    exit(1)

try:
    print yellow % 'You clipboard contents:'
    content = clipboard_get()
    print content
    assert content.strip(), 'Nothing on clipboard.'
except:
    attn('Nothing on clipboard.')
    exit(1)


friends = {
    'hanna': 'hanna@dirichlet.net',
    'tim': 'tim.f.vieira@gmail.com',
    'desktop': 'desktop',
}

try:
    recipient = friends[argv[1]]
except KeyError:
    exit(1)

print red % 'send to:', yellow % recipient

if recipient == 'desktop':
    name = find_new_title('/home/timv/Desktop/', 'paste (001).txt')
    if str2bool(raw_input('%s %s' % (red % '>>>', yellow % 'write file %s? ' % name))):
        with file(name, 'wb') as f:
            f.write(content)

elif str2bool(raw_input('%s %s' % (red % '>>>', yellow % 'Send message? '))):
    send_gmail(gmail_user='timsfanmail',
               recipient=recipient,
               subject='test subject',
               body=content)
else:
    pass  # do nothing..
