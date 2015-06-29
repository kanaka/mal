library step3_env_test;

import "../step3_env.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), repl_env);

void main() {
  // Testing REPL_ENV
  expectResults(testEval, "(+ 1 2)", "3");
  expectResults(testEval, "(/ (- (+ 5 (* 2 3)) 3) 4)", "2");

  // Testing def!
  expectResults(testEval, "(def! x 3)", "3");
  expectResults(testEval, "x", "3");
  expectResults(testEval, "(def! x 4)", "4");
  expectResults(testEval, "x", "4");
  expectResults(testEval, "(def! y (+ 1 7))", "8");
  expectResults(testEval, "y", "8");

  // Testing let*
  expectResults(testEval, "(let* (z 9) z)", "9");
  expectResults(testEval, "(let* (x 9) x)", "9");
  expectResults(testEval, "x", "4");
  expectResults(testEval, "(let* (z (+ 2 3)) (+ 1 z))", "6");
  expectResults(testEval, "(let* (p (+ 2 3) q (+ 2 p)) (+ p q))", "12");

  // Testing outer environment
  expectResults(testEval, "(def! a 4)", "4");
  expectResults(testEval, "(let* (q 9) q)", "9");
  expectResults(testEval, "(let* (q 9) a)", "4");
  expectResults(testEval, "(let* (z 2) (let* (q 9) a))", "4");

  // -------- Optional Functionality --------

  // Testing let* with vector bindings
  expectResults(testEval, "(let* [z 9] z)", "9");
  expectResults(testEval, "(let* [p (+ 2 3) q (+ 2 p)] (+ p q))", "12");

  // Testing vector evaluation
  expectResults(testEval, "(let* (a 5 b 6) [3 4 a [b 7] 8])", "[3 4 5 [6 7] 8]");
}
