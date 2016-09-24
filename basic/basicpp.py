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
    parser.add_argument('--skip-misc-fixups', action='store_true', default=False,
                        help='Skip miscellaneous fixup/shrink fixups')
    parser.add_argument('--number-lines', action='store_true', default=False,
                        help='Number the lines')
    parser.add_argument('--keep-labels', action='store_true', default=False,
                        help='Keep string labels instead of replacing with line numbers')
    parser.add_argument('--combine-lines', action='store_true', default=False,
                        help='Combine lines using the ":" separator')

    args = parser.parse_args()
    if args.combine_lines and args.keep_rems:
        parser.error("--combine-lines and --keep-rems are mutually exclusive")

    return args

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

def misc_fixups(orig_lines):
    text = "\n".join(orig_lines)
    text = re.sub(r"\bTHEN GOTO\b", r"THEN", text)
    return text.split("\n")

def finalize(lines, args):
    labels_lines = {}
    lines_labels = {}

    # number lines
    if args.number_lines:
        src_lines = lines
        lines = []
        lnum=1
        for line in src_lines:
            if not args.keep_labels:
                m = re.match(r"^ *([^ ]*): *$", line)
                if m:
                    labels_lines[m.groups(1)[0]] = lnum
                    lines_labels[lnum] = m.groups(1)[0]
                    continue
            lines.append("%s %s" % (lnum, line))
            lnum += 1

    if not args.keep_labels:
        src_lines = lines
        text = "\n".join(lines)
        # search for and replace GOTO/GOSUBs
        for label, lnum in labels_lines.items():
            stext = ""
            while stext != text:
                stext = text
                text = re.sub(r"(THEN) %s\b" % label, r"THEN %s" % lnum, stext)
                text = re.sub(r"(ON [^:]* GOTO [^:]*)\b%s\b" % label, r"\g<1>%s" % lnum, text)
                text = re.sub(r"(ON [^:]* GOSUB [^:]*)\b%s\b" % label, r"\g<2>%s" % lnum, text)
                text = re.sub(r"(GOSUB) %s\b" % label, r"\1 %s" % lnum, text)
                text = re.sub(r"(GOTO) %s\b" % label, r"\1 %s" % lnum, text)
        lines = text.split("\n")

    if args.combine_lines:
        src_lines = lines
        lines = []
        pos = 0
        acc_line = ""
        while pos < len(src_lines):
            line = src_lines[pos]
            # TODO: handle args.keep_labels and (not args.number_lines)
            m = re.match(r"^([0-9]*) (.*)$", line)
            lnum = int(m.group(1))
            rest_line = m.group(2)

            if acc_line == "":
                # Starting a new line
                acc_line = line
            elif lnum in lines_labels:
                # This is a GOTO/GOSUB target line so it must be on
                # a line by itself
                lines.append(acc_line)
                acc_line = line
            elif re.match(r".*\b(?:GOTO|THEN|RETURN)\b.*", acc_line):
                lines.append(acc_line)
                acc_line = line
            elif len(acc_line) + 1 + len(rest_line) < 80:
                # Continue building up the line
                acc_line = acc_line + ":" + rest_line
                # GOTO/IF/RETURN must be the last things on a line so
                # start a new line
                if re.match(r".*\b(?:GOTO|THEN|RETURN)\b.*", line):
                    lines.append(acc_line)
                    acc_line = ""
            else:
                # Too long so start a new line
                lines.append(acc_line)
                acc_line = line
            pos += 1
        if acc_line != "":
            lines.append(acc_line)


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

    # apply some miscellaneous simple fixups/regex transforms
    if not args.skip_misc_fixups:
        lines = misc_fixups(lines)

    # number lines, drop/keep labels, combine lines
    lines = finalize(lines, args)

    print("\n".join(lines))
