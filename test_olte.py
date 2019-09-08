#! /usr/bin/python3

import unittest

from olte import OneLineTerminalEmulator

class TestOLTE(unittest.TestCase):
    def test_empty(self):
        o = OneLineTerminalEmulator()
        self.assertEqual(o.current_line, "")        
    def test_trivial(self):
        o = OneLineTerminalEmulator()
        o.process("Hello")
        self.assertEqual(o.current_line, "Hello")
    def test_cr(self):
        o = OneLineTerminalEmulator()
        o.process("Hello\rWorld")
        self.assertEqual(o.current_line, "World")
    def test_bs(self):
        o = OneLineTerminalEmulator()
        o.process("Hello\bWorld")
        self.assertEqual(o.current_line, "HellWorld")
    def test_bel(self):
        # BEL should have no effect (and not appear in the output).
        o = OneLineTerminalEmulator()
        o.process("Hello\aWorld")
        self.assertEqual(o.current_line, "HelloWorld")
    def test_lf(self):
        o = OneLineTerminalEmulator()
        o.process("Hello\r\nWorld\r\n")
        self.assertEqual(o.past_lines, ["Hello", "World"])

if __name__ == '__main__':
    unittest.main()
