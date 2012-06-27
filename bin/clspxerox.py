#!/usr/bin/env python
"""
Usage:

Print multiple files

   $ clspxerox.py [--submit] file1.pdf file2.pdf ...

Interactively select a single file

   $ clspxerox.py

"""

import httplib
from sys import argv
from selenium.webdriver import Firefox
from selenium.webdriver.common.keys import Keys
from os.path import abspath

def print_doc(*docs):
    b = Firefox()
    b.get('http://192.168.51.251:8000')
    b.find_element_by_name('deptid').send_keys('1234')
    b.find_element_by_name('password').send_keys('1234')
    b.find_element_by_name('login').submit()

    for d in docs or [None]:
        b.get('http://192.168.51.251:8000/direct')

        f = b.find_element_by_id('File')
        if d:
            try:
                f.send_keys(abspath(d))
            except httplib.BadStatusLine:   # in case send keys fails
                f.click()  # interactively pop-up the file selection menu
        else:
            f.click()  # interactively pop-up the file selection menu

        b.find_element_by_css_selector('select#ColorMode').send_keys('B' + Keys.ENTER)
        b.find_element_by_css_selector('input#DuplexMode.Checkbox').click()
        b.find_element_by_css_selector('select#Sort').send_keys('S' + Keys.ENTER)
        if submit:
            b.find_element_by_css_selector('input[value="Start Printing "]').click()

if __name__ == '__main__':

    try:
        argv.remove('--submit')
    except ValueError:
        submit = False
    else:
        submit = True

    if '-h' in argv or '--help' in argv:
        print __doc__
    else:
        print_doc(*argv[1:])
