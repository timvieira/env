#!/usr/bin/env python

import sys, os, time, atexit
from signal import SIGTERM 

import subprocess
from subprocess import Popen



class Daemon:
    """ A generic daemon class.
        Usage: subclass the Daemon class and override the run() method
    """
    def __init__(self, pidfile, stdin='/dev/null', stdout='/dev/null', stderr='/dev/null'):
        self.stdin = stdin
        self.stdout = stdout
        self.stderr = stderr
        self.pidfile = pidfile
    
    def daemonize(self):
        """
        do the UNIX double-fork magic, see Stevens' "Advanced 
        Programming in the UNIX Environment" for details (ISBN 0201563177)
        http://www.erlenstar.demon.co.uk/unix/faq_2.html#SEC16
        """
        try: 
            pid = os.fork() 
            if pid > 0:
                # exit first parent
                sys.exit(0) 
        except OSError, e: 
            sys.stderr.write("fork #1 failed: %d (%s)\n" % (e.errno, e.strerror))
            sys.exit(1)
    
        # decouple from parent environment
        os.chdir("/") 
        os.setsid() 
        os.umask(0) 
    
        # do second fork
        try: 
            pid = os.fork() 
            if pid > 0:
                # exit from second parent
                sys.exit(0) 
        except OSError, e: 
            sys.stderr.write("fork #2 failed: %d (%s)\n" % (e.errno, e.strerror))
            sys.exit(1) 
    
        # redirect standard file descriptors
        sys.stdout.flush()
        sys.stderr.flush()

        (si,so,se) = (file(self.stdin, 'r'),
                      file(self.stdout, 'a+'),
                      file(self.stderr, 'a+', 0))
        os.dup2(si.fileno(), sys.stdin.fileno())
        os.dup2(so.fileno(), sys.stdout.fileno())
        os.dup2(se.fileno(), sys.stderr.fileno())
    
        # write pidfile
        atexit.register(self.delpid)
        pid = str(os.get_pid())
        file(self.pidfile,'w+').write("%s\n" % pid)
    
    def delpid(self):
        os.remove(self.pidfile)

    def get_pid(self):
        """ get process pid or None """
        pid = None
        try:
            pf = file(self.pidfile,'r')
            pid = int(pf.read().strip())
            pf.close()
        except IOError:
            pass
        return pid

    def start(self):
        """ Start the daemon """
        pid = self.get_pid()
        if pid:
            message = "pidfile %s already exist. Daemon already running?\n"
            sys.stderr.write(message % self.pidfile)
            sys.exit(1)
        
        self.daemonize()
        self.run()

    def foreground(self):
        """ call run in foreground (for debugging) """
        self.run()

    def stop(self):
        """ Stop the daemon """
        pid = self.get_pid()
        if not pid:
            message = "pidfile %s does not exist. Daemon not running?\n"
            sys.stderr.write(message % self.pidfile)
            return # not an error in a restart

        self.kill()

    def kill(self):
        try:
            pid = self.get_pid()
            while true:
                os.kill(pid, SIGTERM)
                time.sleep(0.1)
        except OSError, err:
            err = str(err)
            if err.find("No such process") > 0:
                if os.path.exists(self.pidfile):
                    self.delpid()
            else:
                print str(err)
                sys.exit(1)

    def restart(self):
        """ Restart the daemon """
        self.stop()
        self.start()

    def run(self):
        """
        You should override this method when you subclass Daemon. It will be called after the process has been
        daemonized by start() or restart().
        """




def parse_args():
    from optparse import OptionParser
    usage = "usage: %prog [options] directory? filespec? pattern?"
    parser = OptionParser(usage)

    argspecs = [
        [ "--pidfile"          , "",  "pidfile",  "store",           '/tmp/Daemon.pid'], 
        [ "--start"            , "",  "start",    "store_true",      False],
        [ "--stop"             , "",  "stop",     "store_true",      False],
        [ "--restart"          , "",  "restart",  "store_true",      False],
        [ "--fg"               , "",  "fg",       "store_true",      False],
        [ "--command"          , "",  "command",  "store",           None]]


    for s in argspecs: 
        parser.add_option(s[0], help=s[1], dest=s[2], action=s[3], default=s[4])

    (opts, args) = parser.parse_args()

    dinfile = opts.pidfile + ".in"
    doutfile = opts.pidfile + ".out"

    daemon = MyDaemon(pidfile=opts.pidfile, 
                      stdin=dinfile, 
                      stdout=doutfile)
    try:
        if True in [opts.start, opts.restart, opts.fg]:
            if opts.start:
                daemon.start()
            elif opts.stop: 
                daemon.stop()
            elif opts.restart:
                daemon.stop()
                daemon.start()
            elif opts.fg:
                daemon.foreground()
        elif opts.stop: 
            daemon.stop()
        else:
            send_command(dinfile, doutfile, opts.command)
    except Exception, e:
        print e


def send_command(dinfile, doutfile, command):
    daemon_in = open(dinfile, 'w+')
    daemon_out = open(doutfile, 'r+')
    daemon_in.write(command + '\n')
    daemon_in.close()
    print read_to_prompt(daemon_out)
    daemon_out.close()


def read_to_prompt(ostr):
    lines = ''
    line = ''
    while line.find('(fcsh)') != 0:
        c = ostr.read(1)
        if c == '\n':
            lines += (line + '\n')
            line = ''
        else:
            line += c

    lines += (line + '\n')
    return lines

class MyDaemon(Daemon):
    def start_shell(self):
        os.mkfifo(self.stdin)
        os.mkfifo(self.stdout)

        self.fcsh_shell = Popen( "fcsh", 
                                 bufsize=0, # 0=unbuffered, 1=line-buffered, -n=fully-buffered
                                 stdin=subprocess.PIPE, 
                                 stdout=subprocess.PIPE, 
                                 stderr=subprocess.STDOUT, 
                                 shell=True, 
                                 cwd=None, 
                                 env=None)


        self.stdout_write_and_close("...started fcsh shell w/pid: %s\n%s\n%s" % (str(self.fcsh_shell.pid),
                                      self.eat_startup_message(),
                                      read_to_prompt(self.fcsh_shell.stdout)))


        try:
            while True:
                input = ''
                while not input:
                    input = self.read_user_input()
                    time.sleep(1)
    
                self.fcsh_shell.stdin.write(input)
                self.fcsh_shell.stdin.write('\n')
                self.stdout_write_and_close(read_to_prompt(self.fcsh_shell.stdout))
        finally:
            os.unlink(self.stdin)
            os.unlink(self.stdout)


    def stdout_write_and_close(self, message):
        out = file(self.stdout, 'a+')
        out.write(message)
        out.close()
        
    def read_user_input(self):
        userin = open(self.stdin, 'r+')
        input = userin.readline().strip()
        userin.close()
        return input
        
    def eat_startup_message(self):
        lines = ''
        line = ''
        while line.find('Copyright') != 0:
            line = self.fcsh_shell.stdout.readline()
            lines += line
        return lines
        
    def run(self):
        self.start_shell()

if __name__=='__main__':
    parse_args()
