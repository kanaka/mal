#!/usr/bin/env python

import os, sys, re
import argparse

# http://pexpect.sourceforge.net/pexpect.html
from pexpect import spawn, EOF, TIMEOUT

# TODO: do we need to support '\n' too
sep = "\r\n"
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
parser.add_argument('--redirect', action='store_true',
        help="Run implementation in bash and redirect output to /dev/null")

parser.add_argument('test_file', type=argparse.FileType('r'),
        help="a test file formatted as with mal test data")
parser.add_argument('mal_cmd', nargs="*",
        help="Mal implementation command line. Use '--' to "
             "specify a Mal command line with dashed options.")

args = parser.parse_args(sys.argv[1:])
test_data = args.test_file.read().split('\n')

if args.rundir: os.chdir(args.rundir)

if args.redirect:
    # Redirect to try and force raw mode (no ASCII codes)
    p = spawn('/bin/bash -c "' + " ".join(args.mal_cmd) + ' |tee /dev/null"')
else:
    p = spawn(args.mal_cmd[0], args.mal_cmd[1:])


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
    idx = p.expect(['user> ', 'mal-user> ', EOF, TIMEOUT],
                timeout=timeout)
    if idx not in [0,1]:
        print "Did not get 'user> ' or 'mal-user> ' prompt"
        print "    Got      : %s" % repr(p.before)
        sys.exit(1)


# Wait for the initial prompt
assert_prompt(args.start_timeout)

# Send the pre-eval code if any
if args.pre_eval:
    sys.stdout.write("RUNNING pre-eval: %s" % args.pre_eval)
    p.sendline(args.pre_eval)
    assert_prompt(args.test_timeout)

fail_cnt = 0

while test_data:
    form, out, ret, line_num = read_test(test_data)
    if form == None:
        break
    sys.stdout.write("TEST: %s -> [%s,%s]" % (form, repr(out), repr(ret)))
    sys.stdout.flush()
    expected = "%s%s%s%s" % (form, sep, out, ret)

    p.sendline(form)
    try:
        idx = p.expect(['\r\nuser> ', '\nuser> ',
                        '\r\nmal-user> ', '\nmal-user> '],
                       timeout=args.test_timeout)
        #print "%s,%s,%s" % (idx, repr(p.before), repr(p.after))
        if ret == "*" or p.before == expected:
            print " -> SUCCESS"
        else:
            print " -> FAIL (line %d):" % line_num
            print "    Expected : %s" % repr(expected)
            print "    Got      : %s" % repr(p.before)
            fail_cnt += 1
    except EOF:
        print "Got EOF"
        sys.exit(1)
    except TIMEOUT:
        print "Got TIMEOUT, received: %s" % repr(p.before)
        sys.exit(1)

if fail_cnt > 0:
    print "FAILURES: %d" % fail_cnt
    sys.exit(2)
sys.exit(0)
