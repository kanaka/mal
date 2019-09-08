# OLTE: One-Line Terminal Emulator

# OLTE is a simple terminal emulator library that emulates a terminal
# one line high and infinitely wide.  It's intended for test suites
# that want to drive programs with command-line editing and the like.

import re

class OneLineTerminalEmulator(object):
    # There are three regular expressions here for each kind of magic
    # character sequence that we recognise.  The first recognises a
    # complete sequence.  The second recognises a partial sequence at
    # the end of a string (so there might be more to come).  The third
    # recognises any incomplete sequence (and hence an error if the
    # second didn't also match).

    # Escape sequences (ECMA-35 clause 13):
    escape_sequence = re.compile(r"\x1b[\x20-\x2f]*[\x30-\x7e]")
    partial_escape_sequence = re.compile(r"\x1b[\x20-\x2f]*$")
    invalid_escape_sequence = re.compile(r"\x1b[\x20-\x2f]*")
    # Two-byte representations of C1 controls:
    sevenbit_c1 = re.compile(r"\x1b([\x40-\x5f])")
    # Control sequences (ECMA-48 clause 5.4):
    control_sequence = re.compile(r"\x9b[\x30-\x3f]*[\x20-\x2f]*[\x40-\x7e]")
    partial_control_sequence = re.compile(r"\x9b[\x30-\x3f]*[\x20-\x2f]*$")
    invalid_control_sequence = re.compile(r"\x9b[\x30-\x3f]*[\x20-\x2f]*")
    # Command strings (ECMA-48 clause 5.6):
    command_string = re.compile(r"[\x90\x9d-\x9f][\x08-\x0d\x20-\x7e]*\x9c")
    partial_command_string = re.compile(
        r"[\x90\x9d-\x9f][\x08-\x0d\x20-\x7e]*\x1b?$")
    invalid_command_string = re.compile(
        r"[\x90\x9d-\x9f][\x08-\x0d\x20-\x7e]*")
    # Character strings (ECMA-48 clause 5.6):
    character_string = re.compile(r"\x98[^\x98\x9c]*\x9c")
    partial_character_string = re.compile(r"\x98[^\x98\x9c]*\x1b?$")
    invalid_character_string = re.compile(r"\x98[^\x98\x9c]*")
    def __init__(self):
        self.line = []
        self.past_lines = []
        self.acc = ""
        self.pos = 0
    def process(self, data):
        self.acc += data
        for char in self.acc:
            if char == "\r":
                self.pos = 0
            elif char == "\b":
                self.pos -= 1
            elif char == "\n":
                self.emit(''.join(self.line))
                self.line.clear()
            elif char.isprintable():
                if len(self.line) <= self.pos:
                    self.line.extend([" "] * (self.pos - len(self.line) + 1))
                self.line[self.pos] = char
                self.pos += 1
        self.acc = ""
    @property
    def current_line(self):
        return "".join(self.line)
    def emit(self, line):
        self.past_lines.append(self.current_line)
