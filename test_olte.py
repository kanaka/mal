#! /usr/bin/python

from __future__ import unicode_literals

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
    def test_escseq(self):
        self.assertProduces("Hello\x1bpWorld", [], "HelloWorld")        
    def test_csi(self):
        self.assertProduces("Hello\x1b[1mWorld", [], "HelloWorld")
    def test_cuf(self):
        self.assertProduces("Hello\r\x1b[2CWorld", [], "HeWorld")
    def test_el0(self):
        self.assertProduces("Hello World\b\b\b\b\b\b\x9bK", [], "Hello")
    def test_el1(self):
        self.assertProduces("Hello World\b\b\b\b\b\b\b\x1b[1K", [],
                            "      World")
    def test_el2(self):
        self.assertProduces("Hello World\b\b\b\b\b\b\x1b[2K", [], "")
    def test_prompt(self):
        o = OneLineTerminalEmulator()
        o.process("1234567890\r> ")
        self.assertEqual(o.current_prompt, "> ")

class TestRegexps(unittest.TestCase):
    def multi_test(self, specs):
        for spec in specs:
            for x in spec["matches"]:
#               with self.subTest(input=x):
                    self.assertTrue(spec["regex"].match(x))
            for x in spec["matches_not"]:
#               with self.subTest(input=x):
                    self.assertFalse(spec["regex"].match(x))
    def test_regexps(self):
        self.multi_test([
            { "regex": OneLineTerminalEmulator.escape_sequence,
              "matches": ["\x1b0", "\x1b~", "\x1b !A"],
              "matches_not": ["\x1b\x7f", "\x1b\x1b", "\x1b", "Hello\x1bA"] },
            { "regex": OneLineTerminalEmulator.partial_escape_sequence,
              "matches": ["\x1b", "\x1b "],
              "matches_not": ["\x1b0", "Hello"] },
            { "regex": OneLineTerminalEmulator.invalid_escape_sequence,
              "matches": ["\x1b\x1b", "\x1b\n"],
              "matches_not": ["Hello"] },
            { "regex": OneLineTerminalEmulator.control_sequence,
              "matches": ["\x9b12;34H", "\x9b z"],
              "matches_not": ["Hello"] },
            { "regex": OneLineTerminalEmulator.partial_control_sequence,
              "matches": ["\x9b12;3", "\x9b "],
              "matches_not": ["Hello"] },
            ])

if __name__ == '__main__':
    unittest.main()
