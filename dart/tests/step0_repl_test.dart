library step0_repl_test;

import "../step1_read_print.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), "");

void main() {
  expectResults(testEval, 'hello world', 'hello world', isFailing: true);

  expectResults(testEval, 'abcABC123', 'abcABC123');

  expectResults(testEval, """;:() []{}"'*""", """;:() []{}"'*""", isFailing: true);

  // Test long line
  expectResults(testEval, """hello world abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 (;:() []{}"'* ;:() []{}"'* ;:() []{}"'*)""",
"""hello world abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 (;:() []{}"'* ;:() []{}"'* ;:() []{}"'*)""", isFailing: true);
}
