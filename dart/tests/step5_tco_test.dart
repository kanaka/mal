library step5_tco_test;

import "../step5_tco.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), repl_env);

void main() {
  init();

  //;; Testing recursive tail-call function
  expectResults(testEval, '(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))', "MalFunction");

  expectResults(testEval, '(sum2 10 0)', '55');

  expectResults(testEval, '(def! res2 nil)', 'nil');

  expectResults(testEval, '(def! res2 (sum2 10000 0))', '50005000');

  //;; Test recursive non-tail call function
  expectResults(testEval, '(def! sum-to (fn* (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))', "MalFunction");

  expectResults(testEval, '(sum-to 10)', '55');

  //;;; no try* yet, so test completion of side-effects
  expectResults(testEval, '(def! res1 nil)', 'nil');

  //;;; For implementations without their own TCO this should fail and
  //;;; leave res1 unchanged
  expectResults(testEval, '(def! res1 (sum-to 10000))', 'nil', isFailing: true);
}