#!/usr/bin/env python

from __future__ import print_function
import os, sys, re
import argparse, time
import signal, atexit

from subprocess import Popen, STDOUT, PIPE
from select import select

# Pseudo-TTY and terminal manipulation
import pty, array, fcntl, termios

IS_PY_3 = sys.version_info[0] == 3

debug_file = None
log_file = None

def debug(data):
    if debug_file:
        debug_file.write(data)
        debug_file.flush()

def log(data, end='\n'):
    if log_file:
        log_file.write(data + end)
        log_file.flush()
    print(data, end=end)
    sys.stdout.flush()

sep = "\n"
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
        help="Write messages to the named file in addition the screen")
parser.add_argument('--debug-file', type=str,
        help="Write all test interaction the named file")
parser.add_argument('--hard', action='store_true',
        help="Turn soft tests (soft, deferrable, optional) into hard failures")

# Control whether deferrable and optional tests are executed
parser.add_argument('--deferrable', dest='deferrable', action='store_true',
        help="Enable deferrable tests that follow a ';>>> deferrable=True'")
parser.add_argument('--no-deferrable', dest='deferrable', action='store_false',
        help="Disable deferrable tests that follow a ';>>> deferrable=True'")
parser.set_defaults(deferrable=True)
parser.add_argument('--optional', dest='optional', action='store_true',
        help="Enable optional tests that follow a ';>>> optional=True'")
parser.add_argument('--no-optional', dest='optional', action='store_false',
        help="Disable optional tests that follow a ';>>> optional=True'")
parser.set_defaults(optional=True)

parser.add_argument('test_file', type=str,
        help="a test file formatted as with mal test data")
parser.add_argument('mal_cmd', nargs="*",
        help="Mal implementation command line. Use '--' to "
             "specify a Mal command line with dashed options.")
parser.add_argument('--crlf', dest='crlf', action='store_true',
        help="Write \\r\\n instead of \\n to the input")

class Runner():
    def __init__(self, args, no_pty=False, line_break="\n"):
        #print "args: %s" % repr(args)
        self.no_pty = no_pty

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

        self.line_break = line_break

    def read_to_prompt(self, prompts, timeout):
        end_time = time.time() + timeout
        while time.time() < end_time:
            [outs,_,_] = select([self.stdout], [], [], 1)
            if self.stdout in outs:
                new_data = self.stdout.read(1)
                new_data = new_data.decode("utf-8") if IS_PY_3 else new_data
                #print("new_data: '%s'" % new_data)
                debug(new_data)
                # Perform newline cleanup
                self.buf += new_data.replace("\r", "")
                for prompt in prompts:
                    regexp = re.compile(prompt)
                    match = regexp.search(self.buf)
                    if match:
                        end = match.end()
                        buf = self.buf[0:match.start()]
                        self.buf = self.buf[end:]
                        self.last_prompt = prompt
                        return buf
        return None

    def writeline(self, str):
        def _to_bytes(s):
            return bytes(s, "utf-8") if IS_PY_3 else s

        self.stdin.write(_to_bytes(str.replace('\r', '\x16\r') + self.line_break))

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
        f = open(test_file, newline='') if IS_PY_3 else open(test_file)
        self.data = f.read().split('\n')
        self.soft = False
        self.deferrable = False
        self.optional = False

    def next(self):
        self.msg = None
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
                self.msg = line[3:]
                return True
            elif line[0:5] == ";>>> ":     # settings/commands
                settings = {}
                exec(line[5:], {}, settings)
                if 'soft' in settings:
                    self.soft = settings['soft']
                if 'deferrable' in settings and settings['deferrable']:
                    self.deferrable = "\nSkipping deferrable and optional tests"
                    return True
                if 'optional' in settings and settings['optional']:
                    self.optional = "\nSkipping optional tests"
                    return True
                continue
            elif line[0:1] == ";":         # unexpected comment
                raise Exception("Test data error at line %d:\n%s" % (self.line_num, line))
            self.form = line   # the line is a form to send

            # Now find the output and return value
            while self.data:
                line = self.data[0]
                if line[0:3] == ";=>":
                    self.ret = line[3:]
                    self.line_num += 1
                    self.data.pop(0)
                    break
                elif line[0:2] == ";/":
                    self.out = self.out + line[2:] + sep
                    self.line_num += 1
                    self.data.pop(0)
                else:
                    self.ret = ""
                    break
            if self.ret != None: break

        if self.out[-1:] == sep and not self.ret:
            # If there is no return value, output should not end in
            # separator
            self.out = self.out[0:-1]
        return self.form

args = parser.parse_args(sys.argv[1:])
# Workaround argparse issue with two '--' on command line
if sys.argv.count('--') > 0:
    args.mal_cmd = sys.argv[sys.argv.index('--')+1:]

if args.rundir: os.chdir(args.rundir)

if args.log_file:   log_file   = open(args.log_file, "a")
if args.debug_file: debug_file = open(args.debug_file, "a")

r = Runner(args.mal_cmd, no_pty=args.no_pty, line_break="\r\n" if args.crlf else "\n")
t = TestReader(args.test_file)


def assert_prompt(runner, prompts, timeout):
    # Wait for the initial prompt
    header = runner.read_to_prompt(prompts, timeout=timeout)
    if not header == None:
        if header:
            log("Started with:\n%s" % header)
    else:
        log("Did not receive one of following prompt(s): %s" % repr(prompts))
        log("    Got      : %s" % repr(r.buf))
        sys.exit(1)


# Wait for the initial prompt
try:
    assert_prompt(r, ['[^\s()<>]+> '], args.start_timeout)
except:
    _, exc, _ = sys.exc_info()
    log("\nException: %s" % repr(exc))
    log("Output before exception:\n%s" % r.buf)
    sys.exit(1)

# Send the pre-eval code if any
if args.pre_eval:
    sys.stdout.write("RUNNING pre-eval: %s" % args.pre_eval)
    r.writeline(args.pre_eval)
    assert_prompt(r, ['[^\s()<>]+> '], args.test_timeout)

test_cnt = 0
pass_cnt = 0
fail_cnt = 0
soft_fail_cnt = 0
failures = []

class TestTimeout(Exception):
    pass

while t.next():
    if args.deferrable == False and t.deferrable:
        log(t.deferrable)
        break

    if args.optional == False and t.optional:
        log(t.optional)
        break

    if t.msg != None:
        log(t.msg)
        continue

    if t.form == None: continue

    log("TEST: %s -> [%s,%s]" % (repr(t.form), repr(t.out), t.ret), end='')

    # The repeated form is to get around an occasional OS X issue
    # where the form is repeated.
    # https://github.com/kanaka/mal/issues/30
    expects = [".*%s%s%s" % (sep, t.out, re.escape(t.ret)),
               ".*%s.*%s%s%s" % (sep, sep, t.out, re.escape(t.ret))]

    r.writeline(t.form)
    try:
        test_cnt += 1
        res = r.read_to_prompt(['\r\n[^\s()<>]+> ', '\n[^\s()<>]+> '],
                                timeout=args.test_timeout)
        #print "%s,%s,%s" % (idx, repr(p.before), repr(p.after))
        if (res == None):
            log(" -> TIMEOUT (line %d)" % t.line_num)
            raise TestTimeout("TIMEOUT (line %d)" % t.line_num)
        elif (t.ret == "" and t.out == ""):
            log(" -> SUCCESS (result ignored)")
            pass_cnt += 1
        elif (re.search(expects[0], res, re.S) or
                re.search(expects[1], res, re.S)):
            log(" -> SUCCESS")
            pass_cnt += 1
        else:
            if t.soft and not args.hard:
                log(" -> SOFT FAIL (line %d):" % t.line_num)
                soft_fail_cnt += 1
                fail_type = "SOFT "
            else:
                log(" -> FAIL (line %d):" % t.line_num)
                fail_cnt += 1
                fail_type = ""
            log("    Expected : %s" % repr(expects[0]))
            log("    Got      : %s" % repr(res))
            failed_test = """%sFAILED TEST (line %d): %s -> [%s,%s]:
    Expected : %s
    Got      : %s""" % (fail_type, t.line_num, t.form, repr(t.out),
                        t.ret, repr(expects[0]), repr(res))
            failures.append(failed_test)
    except:
        _, exc, _ = sys.exc_info()
        log("\nException: %s" % repr(exc))
        log("Output before exception:\n%s" % r.buf)
        sys.exit(1)

if len(failures) > 0:
    log("\nFAILURES:")
    for f in failures:
        log(f)

results = """
TEST RESULTS (for %s):
  %3d: soft failing tests
  %3d: failing tests
  %3d: passing tests
  %3d: total tests
""" % (args.test_file, soft_fail_cnt, fail_cnt,
        pass_cnt, test_cnt)
log(results)

debug("\n") # add some separate to debug log

if fail_cnt > 0:
    sys.exit(1)
sys.exit(0)
