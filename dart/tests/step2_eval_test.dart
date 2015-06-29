library step2_eval_test;

import "../step2_eval.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), repl_env);

void main() {
  // Testing evaluation of arithmetic operations
  expectResults(testEval, "(+ 1 2)", "3");

  expectResults(testEval, "(+ 5 (* 2 3))", "11");

  expectResults(testEval, "(- (+ 5 (* 2 3)) 3)", "8");

  expectResults(testEval, "(/ (- (+ 5 (* 2 3)) 3) 4)", "2");

  expectResults(testEval, "(/ (- (+ 515 (* 222 311)) 302) 27)", "2565");

  expectResults(testEval, "(abc 1 2 3)", "'abc' not found.");


  // -------- Optional Functionality --------

  // Testing evaluation within collection literals
  expectResults(testEval, "[1 2 (+ 1 2)]", "[1 2 3]");

  expectResults(testEval, '{"a" (+ 7 8)}', '{"a" 15}');

  expectResults(testEval, "{:a (+ 7 8)}", "{:a 15}");
}
