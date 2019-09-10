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
    # Characters to strip out everywhere:
    strip_chars = re.compile(r"[\x00\x7f]")
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
    # Specific command sequences:
    CUF = re.compile(r"\x9b([0-9]*)C")
    EL  = re.compile(r"\x9b([0-9]*)K")
    # Single characters
    one_char = re.compile('(?s).')
    def __init__(self):
        self.line = []
        self.past_lines = []
        self.acc = ""
        self.pos = 0
    def match(self, pattern, consume=True):
        # A silly little method to provide something equivalent to Perl's
        # $1, $&, etc.
        self.last_match = pattern.match(self.acc)
        if self.last_match and consume:
            self.acc = self.acc[self.last_match.end():]
        return self.last_match
    def process(self, data):
        self.acc += data
        # ECMA-35 says that DEL should be ignored everywhere.  ECMA-48
        # says the same about NUL.
        self.acc = self.strip_chars.sub("", self.acc)
        # Convert 7-bit C1 controls to 8-bit form.
        def c1_convert(match): return chr(ord(match.group(1)) + 0x40)
        self.acc = self.sevenbit_c1.sub(c1_convert, self.acc)
        while self.acc != '':
            if (self.match(self.partial_escape_sequence,  consume=False) or
                self.match(self.partial_control_sequence, consume=False) or
                self.match(self.partial_command_string,   consume=False) or
                self.match(self.partial_character_string, consume=False)):
                # We can't make sense of the input yet.
                return
            if (self.match(self.CUF)):
                param = int(self.last_match.group(1) or '1')
                self.pos += param
                continue
            if (self.match(self.EL)):
                param = int(self.last_match.group(1) or '0')
                if param == 0: del self.line[self.pos:]
                if param == 1: self.line[:self.pos + 1] = [' '] * (self.pos + 1)
                if param == 2: self.line.clear()
                continue
            if (self.match(self.escape_sequence) or
                self.match(self.control_sequence) or
                self.match(self.command_string) or
                self.match(self.character_string)):
                # For now, ignore valid sequences.
                continue
            if (self.match(self.invalid_escape_sequence) or
                self.match(self.invalid_control_sequence) or
                self.match(self.invalid_command_string) or
                self.match(self.invalid_character_string)):
                # Ignore an invalid sequence.
                continue
            char = self.match(self.one_char).group(0)
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
    @property
    def current_line(self):
        return "".join(self.line)
    def emit(self, line):
        self.past_lines.append(self.current_line)
