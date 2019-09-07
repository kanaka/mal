#! /usr/bin/python3

import unittest

from olte import OneLineTerminalEmulator

class TestOLTE(unittest.TestCase):
    def test_empty(self):
        o = OneLineTerminalEmulator()
        self.assertEqual(''.join(o.line), "")        
    def test_trivial(self):
        o = OneLineTerminalEmulator()
        o.process("Hello")
        self.assertEqual(''.join(o.line), "Hello")
    def test_cr(self):
        o = OneLineTerminalEmulator()
        o.process("Hello\rWorld")
        self.assertEqual(''.join(o.line), "World")
    def test_bs(self):
        o = OneLineTerminalEmulator()
        o.process("Hello\bWorld")
        self.assertEqual(''.join(o.line), "HellWorld")

class AccumulatingOLTE(OneLineTerminalEmulator):
    def __init__(self):
        super().__init__()
        self.lines = []
    def emit(self, line):
        self.lines.append(line)

class TestAccOLTE(unittest.TestCase):
    def test_simple(self):
        o = AccumulatingOLTE()
        o.process("Hello\r\nWorld\r\n")
        self.assertEqual(o.lines, ["Hello", "World"])

if __name__ == '__main__':
    unittest.main()
