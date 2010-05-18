#!/bin/env python

import SocketServer
import BaseHTTPServer
import SimpleHTTPServer
import subprocess
from subprocess import Popen
import httplib
import urllib
import urlparse
import cgi
import sys

############
### Server side
##
targets = {}
fcsh_shell = None
fcsh_stdout = None
fcsh_stdin = None

class CompileHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):
    def do_POST(self):
        try:
            cmd = self.path[1:]
            cl = int(self.headers.getheader('Content-Length'))
            content = self.rfile.read(cl)
            args = cgi.parse_qs(content)
            print 'args', args
            args = dict([(k, v[0]) for (k, v) in args.items()])
            print 'args', args
            command = eval(cmd)
            resp = command(**args)
        except Exception, e:
            resp = "while trying command '" + cmd + "'" + ": exception was " + str(e)

        self.send_response(200)
        self.send_header('Content-type', 'text/plain')
        self.send_header('Content-Length', len(resp))
        self.end_headers()
        self.wfile.write(resp)
        return
    
    
    def show(self, swf):
        proc = Popen( "flashplayer", 
                      swf,
                      bufsize=0, # 0=unbuffered, 1=line-buffered, -n=fully-buffered
                      stdin=subprocess.PIPE, 
                      stdout=subprocess.PIPE, 
                      stderr=subprocess.STDOUT, 
                      shell=True, 
                      cwd=None, 
                      env=None)
        return proc.communicate()[0].read()
    
    
    def compile(self, srcpath, srcfile, output):
        mxmlc = ' '.join(
            [s.strip() 
             for s in 
             """mxmlc 
              +configname=flex 
              -compiler.source-path %s 
              -file-specs=%s
              -output %s
              -compiler.warn-no-type-decl=true
              -compiler.show-actionscript-warnings
              -compiler.show-binding-warnings
              -compiler.show-shadowed-device-font-warnings
              -compiler.show-unused-type-selector-warnings
              -compiler.strict
              """.split()]) % (srcpath, srcfile, output)
        return send(mxmlc)
    
    def recompile(self):
        return send( 'compile 1' )
    
    def send(self, cmd):
        global fcsh_shell
        sin, sout = fcsh_shell.stdin, fcsh_shell.stdout
        sin.write(cmd)
        sin.write('\n')
        sin.flush()
        output = []
        while True:
            l = sout.readline().strip()
            print "> ", l
            output += [l]
            # if l.find('(fcsh)') == 0:
            #     break
    
        return '\n'.join(output)
    
    def start(self):
        global fcsh_shell
        fcsh_shell = Popen( "fcsh", 
                            bufsize=0, # 0=unbuffered, 1=line-buffered, -n=fully-buffered
                            stdin=subprocess.PIPE, 
                            stdout=subprocess.PIPE, 
                            stderr=subprocess.STDOUT, 
                            shell=True, 
                            cwd=None, 
                            env=None)
    
        from threading import Thread
        t = new Thread(target=self.run_fcsh_output, kwargs={'self', self})
    
    
    def run_fcsh_output(self):
        global fcsh_shell
        while True:
            line = fcsh_shell.stdout.readline()
            self.send_response(200)
            self.send_header('Content-type', 'text/plain')
            self.send_header('Content-Length', len(line))
            self.end_headers()
            self.wfile.write(line)

def start_server(self, port):
    server = BaseHTTPServer.HTTPServer(('localhost', port), CompileHandler)
    try:
        print "compile server on port ", port
        server.serve_forever()
    except (KeyboardInterrupt, RuntimeError):
        print "closing compile server"

    
############
### Client side
##

def post(port, url, params):
    try:
        conn = httplib.HTTPConnection("localhost", port)
        content = urllib.urlencode(params)
        conn.request("POST", url, 
                     content, 
                     { "Content-type": "application/x-www-form-urlencoded",
                       "Accept": "text/plain", 
                       "Content-Length": len(content) })
        response = conn.getresponse()
        print "Status: ", response.status, response.reason
        data = response.read()
        print data
    except Exception, e:
        print str(e)
        conn.close()


def parse_args():
    from optparse import OptionParser
    usage = "usage: %prog [options] directory? filespec? pattern?"
    parser = OptionParser(usage)

    argspecs = [
        [ "--port"             , "",  "port",     "store",           '2001'], 
        [ "--server"           , "",  "server",   "store_true",      False], 
        [ "--srcfile"          , "",  "srcfile",  "store",           None], 
        [ "--srcpath"          , "",  "srcpath",  "store",           None], 
        [ "--output"           , "",  "output",   "store",           None], 
        [ "--compile"          , "",  "compile",  "store",           None], 
        [ "--show"             , "",  "show",     "store",           None], 
        [ "--start"            , "",  "start",    "store_true",      False], 
        [ "--exit"             , "",  "exit",     "store_true",      False]]

    for s in argspecs: 
        parser.add_option(s[0], help=s[1], dest=s[2], action=s[3], default=s[4])

    (opts, args) = parser.parse_args()
    port = int(opts.port)
    
    if opts.server:
        start_server(port)
        exit()

    commands = [a for a in 
                'compile show start exit'.split()
                if getattr(opts, a)]

    params = dict([(a, getattr(opts, a)) for a in 
               'srcfile srcpath output'.split()
               if getattr(opts, a)])

    for cmd in commands:
        post(port, "/"+cmd, params)

def main():
    parse_args()

if __name__=='__main__':
    main()



## All of mxmlc options
#  -benchmark
#  -compiler.accessible
#  -compiler.actionscript-file-encoding <string>
#  -compiler.context-root <context-path>
#  -compiler.debug
#  -compiler.external-library-path [path-element] [...]
#  -compiler.fonts.advanced-anti-aliasing
#  -compiler.fonts.flash-type
#  -compiler.fonts.max-glyphs-per-face <string>
#  -compiler.include-libraries [library] [...]
#  -compiler.incremental
#  -compiler.library-path [path-element] [...]
#  -compiler.locale [locale-element] [...]
#  -compiler.mxml.compatibility-version <version>
#  -compiler.namespaces.namespace <uri> <manifest>
#  -compiler.optimize
#  -compiler.services <filename>
#  -compiler.show-actionscript-warnings
#  -compiler.show-binding-warnings
#  -compiler.show-shadowed-device-font-warnings
#  -compiler.show-unused-type-selector-warnings
#  -compiler.source-path [path-element] [...]
#  -compiler.strict
#  -compiler.theme [filename] [...]
#  -compiler.use-resource-bundle-metadata
#  -help [keyword] [...]
#  -include-resource-bundles [bundle] [...]
#  -licenses.license <product> <serial-number>
#  -load-config <filename>
#  -metadata.contributor <name>
#  -metadata.creator <name>
#  -metadata.date <text>
#  -metadata.description <text>
#  -metadata.language <code>
#  -metadata.localized-description <text> <lang>
#  -metadata.localized-title <title> <lang>
#  -metadata.publisher <name>
#  -metadata.title <text>
#  -output <filename>
#  -runtime-shared-libraries [url] [...]
#  -runtime-shared-library-path [path-element] [rsl-url] [policy-file-url] [rsl-url] [policy-file-url]
#  -static-link-runtime-shared-libraries
#  -target-player <version>
#  -use-network
#  -version
#  -warnings



