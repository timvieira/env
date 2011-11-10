#!/usr/bin/env python

"""
Command-line interface to Google Calendar's quick add feature.

Requires gdata package <http://pypi.python.org/pypi/gdata>
"""

import sys, webbrowser
from getpass import getpass

from atom import Content
from gdata.calendar import CalendarEventEntry, QuickAdd
from gdata.calendar.service import CalendarService

def quickadd(email, content, open_url=True):

    client = CalendarService(email, getpass(), 'Google-Calendar_Python_Sample-1.0')
    client.ProgrammaticLogin()

    event = CalendarEventEntry()
    event.content = Content(text=content)
    event.quick_add = QuickAdd(value='true')
    new_event = client.InsertEvent(event, '/calendar/feeds/default/private/full')

    print 'created event: %r' % new_event.title.text
    if open_url:
        webbrowser.open(new_event.link[0].href)

if __name__ == '__main__':
    quickadd('timsfanmail', ' '.join(sys.argv[1:]))
