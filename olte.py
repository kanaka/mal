# OLTE: One-Line Terminal Emulator

# OLTE is a simple terminal emulator library that emulates a terminal
# one line high and infinitely wide.  It's intended for test suites
# that want to drive programs with command-line editing and the like.

class OneLineTerminalEmulator(object):
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
