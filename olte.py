# OLTE: One-Line Terminal Emulator

# OLTE is a simple terminal emulator library that emulates a terminal
# one line high and infinitely wide.  It's intended for test suites
# that want to drive programs with command-line editing and the like.

class OneLineTerminalEmulator(object):
    def __init__(self):
        self.line = []
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
    def emit(self, line):
        """
        This method is called whenever a line is scrolled off the top
        of the terminal (by LF or similar).  Override it to actually do
        something with complete lines.
        """
        pass
