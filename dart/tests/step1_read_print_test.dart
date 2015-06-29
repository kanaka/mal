library step1_read_print_test;

import "../step1_read_print.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), "");

void main() {
  // Testing read of nil/true/false
  expectResults(testEval, 'nil', 'nil');
  expectResults(testEval, 'true', 'true');
  expectResults(testEval, 'false', 'false');

  // Testing read of numbers
  expectResults(testEval, '1', '1');
  expectResults(testEval, '7', '7');
  expectResults(testEval, '7', '7');

  // Testing read of symbols
  expectResults(testEval, '+', '+');
  expectResults(testEval, 'abc', 'abc');
  expectResults(testEval, 'abc', 'abc');
  expectResults(testEval, 'abc5', 'abc5');
  expectResults(testEval, 'abc-def', 'abc-def');

  // Testing read of strings
  expectResults(testEval, '"abc"', 'abc');
  expectResults(testEval, '   "abc"', 'abc');
  expectResults(testEval, '"abc (with parens)"', 'abc (with parens)');
  expectResults(testEval, '"abc"def"', '"abc"def"', isFailing: true);
  expectResults(testEval, '""', '');

  // Testing read of lists
  expectResults(testEval, '(+ 1 2)', '(+ 1 2)');
  expectResults(testEval, '((3 4))', '((3 4))');
  expectResults(testEval, '(+ 1 (+ 2 3))', '(+ 1 (+ 2 3))');
  expectResults(testEval, '( +   1   (+   2 3   )   )', '(+ 1 (+ 2 3))');
  expectResults(testEval, '(* 1 2)', '(* 1 2)');
  expectResults(testEval, '(** 1 2)', '(** 1 2)');

  // Test commas as whitespace
  expectResults(testEval, '(1 2, 3,,,,),,', '(1 2 3)');

  // Testing read of quoting
  expectResults(testEval, "'1'", '(quote 1)');
  expectResults(testEval, "'(1 2 3)'", '(quote (1 2 3))');
  expectResults(testEval, '`1', '(quasiquote 1)');
  expectResults(testEval, '`(1 2 3)', '(quasiquote (1 2 3))');
  expectResults(testEval, '~1', '(unquote 1)');
  expectResults(testEval, '~(1 2 3)', '(unquote (1 2 3))');
  expectResults(testEval, '~@(1 2 3)', '(splice-unquote (1 2 3))');

  // Testing reader errors
  expectResults(testEval, "(1 2", "expected ')', got EOF");

  expectResults(testEval, "[1 2", "expected ']', got EOF");

  expectResults(testEval, '"abc', """expected '"', got EOF""", isFailing: true);

  // -------- Optional Functionality --------

  // Testing keywords
  expectResults(testEval, ':kw', ':kw');
  expectResults(testEval, '(:kw1 :kw2 :kw3)', '(:kw1 :kw2 :kw3)');

  // Testing read of vectors
  expectResults(testEval, '[+ 1 2]', '[+ 1 2]');
  expectResults(testEval, '[[3 4]]', '[[3 4]]');
  expectResults(testEval, '[+ 1 [+ 2 3]]', '[+ 1 [+ 2 3]]');
  expectResults(testEval, '[ +   1   [+   2 3   ]   ]', '[+ 1 [+ 2 3]]');

  // Testing read of hash maps
  expectResults(testEval, '{"abc" 1}', '{"abc" 1}');
  expectResults(testEval, '{"a" {"b" 2}}', '{"a" {"b" 2}}');
  expectResults(testEval, '{"a" {"b" {"c" 3}}}', '{"a" {"b" {"c" 3}}}');
  expectResults(testEval, '{  "a"  {"b"   {  "cde"     3   }  }}', '{"a" {"b" {"cde" 3}}}');
  expectResults(testEval, '{  :a  {:b   {  :cde     3   }  }}', '{:a {:b {:cde 3}}}');

  // Testing read of comments
  // whole line comment (not an exception)
  expectResults(testEval, '1 ; comment after expression', '1');
  expectResults(testEval, '1; comment after expression', '1');

  // Testing read of ^/metadata
  expectResults(testEval, '^{"a" 1} [1 2 3]', '(with-meta [1 2 3] {"a" 1})');

  // Testing read of @/deref
  expectResults(testEval, '@a', '(deref a)');
}
