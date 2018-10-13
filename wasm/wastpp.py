#!/usr/bin/env python3

from itertools import tee
from ast import literal_eval
import os
import pprint
import re
import sys

def pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = tee(iterable)
    next(b, None)
    return zip(a, b)

def _escape(s):
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')


tokre = re.compile(r"""([\s][\s]*|[(];|;[)]|[\[\]{}()`~^@]|'(?:[\\].|[^\\'])*'?|"(?:[\\].|[^\\"])*"?|;;.*|[^\s\[\]{}()'"`@,;]+)""")

file_tokens = []
strings = []
string_map = {}

depth = 0
module = None
type = None

for f in sys.argv[1:]:
    content = open(f).read()
    tokens = [t for t in re.findall(tokre, content)]
    #print(tokens[0:100], file=sys.stderr)
    pairs = ["".join(p) for p in pairwise(tokens)]
    pairs.append("")

    index = 0
    while index < len(tokens):
        token = tokens[index]
        pair = pairs[index]
        if pair in ["(STRING", "(CHAR"]:
            arg = tokens[index+3]
            #print("arg: %s" % arg, file=sys.stderr)
            if tokens[index+4] != ')':
                raise Exception("Invalid %s) macro, too many/few args" % pair)
            if arg.startswith('"') and arg.endswith('"'):
                pass
            elif arg.startswith("'") and arg.endswith("'"):
                pass
            else:
                raise Exception ("Invalid %s) macro, invalid string arg" % pair)
            if pair == "(STRING":
                str = literal_eval(arg)
                if str in string_map:
                    # Duplicate string, re-use address
                    file_tokens.append("(i32.add (get_global $memoryBase) (get_global %s))" % string_map[str])
                else:
                    str_name = "$S_STRING_%d" % len(strings)
                    file_tokens.append("(i32.add (get_global $memoryBase) (get_global %s))" % str_name)
                    strings.append(str)
                    string_map[str] = str_name
            if pair == "(CHAR":
                c = literal_eval(arg)
                if len(c) != 1:
                    raise Exception ("Invalid (CHAR) macro, must be 1 character")
                file_tokens.append("(i32.const 0x%x (; %s ;))" % (ord(c), arg))
            # Skip the rest of the macro
            index += 5
            continue
        index += 1
        if token == '(':
            depth += 1
        if token == ')':
            depth -= 1
        if depth == 0:
            module = None
            if token == ')': continue
        if depth == 1:
            type = None
            if pair == '(module':
                index += 1
                continue
            if token.startswith('$'):
                module = token[1:]
                #print("module:", module, file=sys.stderr)
                file_tokens.append('\n  ;;\n  ;; module "%s"\n  ;;\n' % module)
                continue
        if depth == 2:
            if token == '(':
                type = tokens[index]
                if type == 'data':
                    raise Exception("User data section not supported")
                #print("  type:", type, file=sys.stderr)
        file_tokens.append(token)

# TODO: remove duplicates
# Create data section with static strings
string_tokens = []
if strings:
    string_tokens.append("  (data\n    (get_global $memoryBase)\n")
    string_offset = 0
    for string in strings:
        string_tokens.append('    %-30s ;; %d\n' % (
            '"'+_escape(string)+'\\00"', string_offset))
        string_offset += len(string)+1
    string_tokens.append("  )\n\n")

    # Create string names/pointers
    string_offset = 0
    for index, string in enumerate(strings):
        string_tokens.append('  (global $S_STRING_%d  i32 (i32.const %d))\n' % (
            index, string_offset))
        string_offset += len(string)+1
    # Terminator so we know how much memory we took
    string_tokens.append('  (global $S_STRING_END  i32 (i32.const %d))\n' % (
        string_offset))

all_tokens = ["(module\n"]
all_tokens.extend(string_tokens)
all_tokens.extend(file_tokens)
all_tokens.append("\n)")

print("".join(all_tokens))
