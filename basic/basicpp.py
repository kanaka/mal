#!/usr/bin/env python

from __future__ import print_function
import argparse
import re
import sys

def debug(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def parse_args():
    parser = argparse.ArgumentParser(description='Preprocess Basic code.')
    parser.add_argument('infile', type=str,
                        help='the Basic file to preprocess')
    parser.add_argument('--keep-rems', action='store_true', default=False,
                        help='The type of REMs to keep (0 (none) -> 4 (all)')
    parser.add_argument('--keep-blank-lines', action='store_true', default=False,
                        help='Keep blank lines from the original file')
    parser.add_argument('--keep-indent', action='store_true', default=False,
                        help='Keep line identing')
    parser.add_argument('--number-lines', action='store_true', default=False,
                        help='Number the lines')
    parser.add_argument('--keep-labels', action='store_true', default=False,
                        help='Keep string labels instead of replacing with line numbers')

    return parser.parse_args()

# pull in include files
def resolve_includes(orig_lines, keep_rems=0):
    included = {}
    lines = []
    for line in orig_lines:
        m = re.match(r"^ *REM \$INCLUDE: '([^']*)' *$", line)
        if m and m.group(1) not in included:
            f = m.group(1)
            if f not in included:
                ilines = [l.rstrip() for l in open(f).readlines()]
                if keep_rems: lines.append("REM vvv BEGIN '%s' vvv" % f)
                lines.extend(ilines)
                if keep_rems: lines.append("REM ^^^ END '%s' ^^^" % f)
            else:
                debug("Ignoring already included file: %s" % f)
        else:
            lines.append(line)
    return lines

def drop_blank_lines(orig_lines):
    lines = []
    for line in orig_lines:
        if re.match(r"^\w*$", line): continue
        lines.append(line)
    return lines


def drop_rems(orig_lines):
    lines = []
    for line in orig_lines:
        if re.match(r"^ *REM", line):
            continue
        m = re.match(r"^(.*): *REM .*$", line)
        if m:
            lines.append(m.group(1))
        else:
            lines.append(line)
    return lines

def remove_indent(orig_lines):
    lines = []
    for line in orig_lines:
        m = re.match(r"^ *([^ ].*)$", line)
        lines.append(m.group(1))
    return lines

def number_lines(orig_lines, keep_labels=True):
    # number lines
    lnum=1
    labels = {}
    lines = []
    for line in orig_lines:
        if not keep_labels:
            m = re.match(r"^ *([^ ]*): *$", line)
            if m:
                labels[m.groups(1)] = lnum
                continue
        lines.append("%s %s" % (lnum, line))
        lnum += 1

    if not keep_labels:
        text = "\n".join(lines)
        # search for and replace GOTO/GOSUBs
        for label, lnum in labels.items():
            text = re.sub(r"(THEN) %s\b" % label, r"THEN %s" % lnum, text)
            text = re.sub(r"(ON [^:]* GOTO [^:]*) %s\b" % label, r"\1 %s" % lnum, text)
            text = re.sub(r"(ON [^:]* GOSUB [^:]*) %s\b" % label, r"\2 %s" % lnum, text)
            text = re.sub(r"(GOSUB) %s\b" % label, r"\1 %s" % lnum, text)
            text = re.sub(r"(GOTO) %s\b" % label, r"\1 %s" % lnum, text)
        return text.split("\n")
    else:
        return lines

if __name__ == '__main__':
    args = parse_args()

    debug("Preprocessing basic file '"+args.infile+"'")

    # read in lines
    lines = [l.rstrip() for l in open(args.infile).readlines()]
    debug("Number of original lines: %s" % len(lines))

    # pull in include files
    lines = resolve_includes(lines, keep_rems=args.keep_rems)
    debug("Number of lines after includes: %s" % len(lines))

    # drop blank lines
    if not args.keep_blank_lines:
        lines = drop_blank_lines(lines)
        debug("Number of lines after dropping blank lines: %s" % len(lines))

    # keep/drop REMs
    if not args.keep_rems:
        lines = drop_rems(lines)
        debug("Number of lines after dropping REMs: %s" % len(lines))

    # keep/remove the indenting
    if not args.keep_indent:
        lines = remove_indent(lines)

    # number lines
    if args.number_lines:
        lines = number_lines(lines, keep_labels=args.keep_labels)

    print("\n".join(lines))
