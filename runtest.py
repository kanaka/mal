#!/usr/bin/env python

import os, sys, re
import argparse, time

import pty, signal, atexit
from subprocess import Popen, STDOUT, PIPE
from select import select

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
parser.add_argument('--mono', action='store_true',
        help="Use workarounds Mono/.Net Console misbehaviors")

parser.add_argument('test_file', type=argparse.FileType('r'),
        help="a test file formatted as with mal test data")
parser.add_argument('mal_cmd', nargs="*",
        help="Mal implementation command line. Use '--' to "
             "specify a Mal command line with dashed options.")

class Runner():
    def __init__(self, args, mono=False):
        #print "args: %s" % repr(args)
        self.mono = mono

        # Cleanup child process on exit
        atexit.register(self.cleanup)

        if mono:
            self.p = Popen(args, bufsize=0,
                           stdin=PIPE, stdout=PIPE, stderr=STDOUT,
                           preexec_fn=os.setsid)
            self.stdin = self.p.stdin
            self.stdout = self.p.stdout
        else:
            # provide tty to get 'interactive' readline to work
            master, slave = pty.openpty()
            self.p = Popen(args, bufsize=0,
                           stdin=slave, stdout=slave, stderr=STDOUT,
                           preexec_fn=os.setsid)
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
                #print "new_data: '%s'" % new_data
                if self.mono:
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

    def writeline(self, str):
        self.stdin.write(str + "\n")
        if self.mono:
            # Simulate echo
            self.buf += str + "\r\n"

    def cleanup(self):
        #print "cleaning up"
        if self.p:
            os.killpg(self.p.pid, signal.SIGTERM)
            self.p = None


args = parser.parse_args(sys.argv[1:])
test_data = args.test_file.read().split('\n')

if args.rundir: os.chdir(args.rundir)

r = Runner(args.mal_cmd, mono=args.mono)


test_idx = 0
def read_test(data):
    global test_idx
    form, output, ret = None, "", None
    while data:
        test_idx += 1
        line = data.pop(0)
        if re.match(r"^\s*$", line):   # blank line
            continue
        elif line[0:3] == ";;;":       # ignore comment
            continue
        elif line[0:2] == ";;":        # output comment
            print line[3:]
            continue
        elif line[0:2] == ";":         # unexpected comment
            print "Test data error at line %d:\n%s" % (test_idx, line)
            return None, None, None, test_idx
        form = line   # the line is a form to send

        # Now find the output and return value
        while data:
            line = data[0]
            if line[0:3] == ";=>":
                ret = line[3:].replace('\\r', '\r').replace('\\n', '\n')
                test_idx += 1
                data.pop(0)
                break
            elif line[0:2] == "; ":
                output = output + line[2:] + sep
                test_idx += 1
                data.pop(0)
            else:
                ret = "*"
                break
        if ret: break

    return form, output, ret, test_idx

def assert_prompt(timeout):
    # Wait for the initial prompt
    header = r.read_to_prompt(['user> ', 'mal-user> '], timeout=timeout)
    if not header == None:
        if header:
            print "Started with:\n%s" % header
    else:
        print "Did not get 'user> ' or 'mal-user> ' prompt"
        print "    Got      : %s" % repr(r.buf)
        sys.exit(1)


# Wait for the initial prompt
assert_prompt(args.start_timeout)

# Send the pre-eval code if any
if args.pre_eval:
    sys.stdout.write("RUNNING pre-eval: %s" % args.pre_eval)
    p.write(args.pre_eval)
    assert_prompt(args.test_timeout)

fail_cnt = 0

while test_data:
    form, out, ret, line_num = read_test(test_data)
    if form == None:
        break
    sys.stdout.write("TEST: %s -> [%s,%s]" % (form, repr(out), repr(ret)))
    sys.stdout.flush()
    expected = "%s%s%s%s" % (form, sep, out, ret)

    r.writeline(form)
    try:
        res = r.read_to_prompt(['\r\nuser> ', '\nuser> ',
                                '\r\nmal-user> ', '\nmal-user> '],
                                timeout=args.test_timeout)
        #print "%s,%s,%s" % (idx, repr(p.before), repr(p.after))
        if ret == "*" or res == expected:
            print " -> SUCCESS"
        else:
            print " -> FAIL (line %d):" % line_num
            print "    Expected : %s" % repr(expected)
            print "    Got      : %s" % repr(res)
            fail_cnt += 1
    except:
        print "Got Exception"
        sys.exit(1)

if fail_cnt > 0:
    print "FAILURES: %d" % fail_cnt
    sys.exit(2)
sys.exit(0)
