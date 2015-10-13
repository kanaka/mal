#!/usr/bin/env python

import os, sys, re
import argparse, time
import signal, atexit

from subprocess import Popen, STDOUT, PIPE
from select import select

# Pseudo-TTY and terminal manipulation
import pty, array, fcntl, termios

IS_PY_3 = sys.version_info[0] == 3

# TODO: do we need to support '\n' too
sep = "\r\n"
#sep = "\n"
rundir = None

parser = argparse.ArgumentParser(
        description="Run a test file against a Mal implementation")
parser.add_argument('--rundir',
        help="change to the directory before running tests")
parser.add_argument('--start-timeout', default=10, type=int,
        help="default timeout for initial prompt")
parser.add_argument('--test-timeout', default=20, type=int,
        help="default timeout for each individual test action")
parser.add_argument('--pre-eval', default=None, type=str,
        help="Mal code to evaluate prior to running the test")
parser.add_argument('--no-pty', action='store_true',
        help="Use direct pipes instead of pseudo-tty")
parser.add_argument('--log-file', type=str,
        help="Write all test interaction the named file")
parser.add_argument('--soft', action='store_true',
        help="Report but do not fail tests after ';>>> soft=True'")

parser.add_argument('test_file', type=argparse.FileType('r'),
        help="a test file formatted as with mal test data")
parser.add_argument('mal_cmd', nargs="*",
        help="Mal implementation command line. Use '--' to "
             "specify a Mal command line with dashed options.")

class Runner():
    def __init__(self, args, no_pty=False, log_file=None):
        #print "args: %s" % repr(args)
        self.no_pty = no_pty

        if log_file: self.logf = open(log_file, "a")
        else:        self.logf = None

        # Cleanup child process on exit
        atexit.register(self.cleanup)

        self.p = None
        env = os.environ
        env['TERM'] = 'dumb'
        env['INPUTRC'] = '/dev/null'
        env['PERL_RL'] = 'false'
        if no_pty:
            self.p = Popen(args, bufsize=0,
                           stdin=PIPE, stdout=PIPE, stderr=STDOUT,
                           preexec_fn=os.setsid,
                           env=env)
            self.stdin = self.p.stdin
            self.stdout = self.p.stdout
        else:
            # provide tty to get 'interactive' readline to work
            master, slave = pty.openpty()

            # Set terminal size large so that readline will not send
            # ANSI/VT escape codes when the lines are long.
            buf = array.array('h', [100, 200, 0, 0])
            fcntl.ioctl(master, termios.TIOCSWINSZ, buf, True)

            self.p = Popen(args, bufsize=0,
                           stdin=slave, stdout=slave, stderr=STDOUT,
                           preexec_fn=os.setsid,
                           env=env)
            # Now close slave so that we will get an exception from
            # read when the child exits early
            # http://stackoverflow.com/questions/11165521
            os.close(slave)
            self.stdin = os.fdopen(master, 'r+b', 0)
            self.stdout = self.stdin

        #print "started"
        self.buf = ""
        self.last_prompt = ""

    def read_to_prompt(self, prompts, timeout):
        end_time = time.time() + timeout
        while time.time() < end_time:
            [outs,_,_] = select([self.stdout], [], [], 1)
            if self.stdout in outs:
                new_data = self.stdout.read(1)
                new_data = new_data.decode("utf-8") if IS_PY_3 else new_data
                #print "new_data: '%s'" % new_data
                self.log(new_data)
                if self.no_pty:
                    self.buf += new_data.replace("\n", "\r\n")
                else:
                    self.buf += new_data
                for prompt in prompts:
                    regexp = re.compile(prompt)
                    match = regexp.search(self.buf)
                    if match:
                        end = match.end()
                        buf = self.buf[0:end-len(prompt)]
                        self.buf = self.buf[end:]
                        self.last_prompt = prompt
                        return buf
        return None

    def log(self, data):
        if self.logf:
            self.logf.write(data)
            self.logf.flush()


    def writeline(self, str):
        def _to_bytes(s):
            return bytes(s, "utf-8") if IS_PY_3 else s

        self.stdin.write(_to_bytes(str + "\n"))

    def cleanup(self):
        #print "cleaning up"
        if self.p:
            try:
                os.killpg(self.p.pid, signal.SIGTERM)
            except OSError:
                pass
            self.p = None

class TestReader:
    def __init__(self, test_file):
        self.line_num = 0
        self.data = test_file.read().split('\n')
        self.soft = False

    def next(self):
        self.form = None
        self.out = ""
        self.ret = None

        while self.data:
            self.line_num += 1
            line = self.data.pop(0)
            if re.match(r"^\s*$", line):   # blank line
                continue
            elif line[0:3] == ";;;":       # ignore comment
                continue
            elif line[0:2] == ";;":        # output comment
                print(line[3:])
                continue
            elif line[0:5] == ";>>> ":     # settings/commands
                settings = {}
                exec(line[5:], {}, settings)
                if 'soft' in settings: self.soft = True
                continue
            elif line[0:1] == ";":         # unexpected comment
                print("Test data error at line %d:\n%s" % (self.line_num, line))
                return None
            self.form = line   # the line is a form to send

            # Now find the output and return value
            while self.data:
                line = self.data[0]
                if line[0:3] == ";=>":
                    self.ret = line[3:].replace('\\r', '\r').replace('\\n', '\n')
                    self.line_num += 1
                    self.data.pop(0)
                    break
                elif line[0:2] == "; ":
                    self.out = self.out + line[2:] + sep
                    self.line_num += 1
                    self.data.pop(0)
                else:
                    self.ret = "*"
                    break
            if self.ret: break

        return self.form

args = parser.parse_args(sys.argv[1:])
# Workaround argparse issue with two '--' on command line
if sys.argv.count('--') > 0:
    args.mal_cmd = sys.argv[sys.argv.index('--')+1:]

if args.rundir: os.chdir(args.rundir)

r = Runner(args.mal_cmd, no_pty=args.no_pty, log_file=args.log_file)
t = TestReader(args.test_file)


def assert_prompt(runner, prompts, timeout):
    # Wait for the initial prompt
    header = runner.read_to_prompt(prompts, timeout=timeout)
    if not header == None:
        if header:
            print("Started with:\n%s" % header)
    else:
        print("Did not one of following prompt(s): %s" % repr(prompts))
        print("    Got      : %s" % repr(r.buf))
        sys.exit(1)


# Wait for the initial prompt
assert_prompt(r, ['user> ', 'mal-user> '], args.start_timeout)

# Send the pre-eval code if any
if args.pre_eval:
    sys.stdout.write("RUNNING pre-eval: %s" % args.pre_eval)
    p.write(args.pre_eval)
    assert_prompt(args.test_timeout)

fail_cnt = 0
soft_fail_cnt = 0

while t.next():
    sys.stdout.write("TEST: %s -> [%s,%s]" % (t.form, repr(t.out), t.ret))
    sys.stdout.flush()

    # The repeated form is to get around an occasional OS X issue
    # where the form is repeated.
    # https://github.com/kanaka/mal/issues/30
    expected = ["%s%s%s%s" % (t.form, sep, t.out, t.ret),
                "%s%s%s%s%s%s" % (t.form, sep, t.form, sep, t.out, t.ret)]

    r.writeline(t.form)
    try:
        res = r.read_to_prompt(['\r\nuser> ', '\nuser> ',
                                '\r\nmal-user> ', '\nmal-user> '],
                                timeout=args.test_timeout)
        #print "%s,%s,%s" % (idx, repr(p.before), repr(p.after))
        if t.ret == "*" or res in expected:
            print(" -> SUCCESS")
        else:
            if args.soft and t.soft:
                print(" -> SOFT FAIL (line %d):" % t.line_num)
                soft_fail_cnt += 1
            else:
                print(" -> FAIL (line %d):" % t.line_num)
                fail_cnt += 1
            print("    Expected : %s" % repr(expected))
            print("    Got      : %s" % repr(res))
    except:
        _, exc, _ = sys.exc_info()
        print("\nException: %s" % repr(exc))
        print("Output before exception:\n%s" % r.buf)
        sys.exit(1)

if soft_fail_cnt > 0:
    print("SOFT FAILURES: %d" % soft_fail_cnt)
if fail_cnt > 0:
    print("FAILURES: %d" % fail_cnt)
    sys.exit(2)
sys.exit(0)
