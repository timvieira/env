#!/usr/bin/env python

import CGIHTTPServer
import BaseHTTPServer

class Handler(CGIHTTPServer.CGIHTTPRequestHandler):
    cgi_directories = ["/cgi-bin"]

port = 80
httpd = BaseHTTPServer.HTTPServer(("", port), Handler)
print "serving at port", port
httpd.serve_forever()
