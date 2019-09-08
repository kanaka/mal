#! /usr/bin/python3

import unittest

from olte import OneLineTerminalEmulator

class TestOLTE(unittest.TestCase):
    def assertProduces(self, input, past, current):
        o = OneLineTerminalEmulator()
        o.process(input)
        self.assertEqual(o.past_lines, past)
        self.assertEqual(o.current_line, current)
    def test_empty(self):
        o = OneLineTerminalEmulator()
        self.assertEqual(o.current_line, "")       
    def test_trivial(self):
        self.assertProduces("Hello", [], "Hello")
    def test_cr(self):
        self.assertProduces("Hello\rWorld", [], "World")
    def test_bs(self):
        self.assertProduces("Hello\bWorld", [], "HellWorld")
    def test_bel(self):
        # BEL should have no effect (and not appear in the output).
        self.assertProduces("Hello\aWorld", [], "HelloWorld")
    def test_lf(self):
        self.assertProduces("Hello\r\nWorld\r\n", ["Hello", "World"], "")

if __name__ == '__main__':
    unittest.main()
