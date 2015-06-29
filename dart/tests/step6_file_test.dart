library step6_file_test;

import "../step6_file.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), repl_env);

void main() {
  init();

  expectResults(testEval, '(eval (read-string (str "(do" (slurp "../../tests/incA.mal") ")")))', "nil", isFailing: false);

  expectResults(testEval, '7 ;; comment', '7');
  expectResults(testEval, '"7 ;; comment"', '7 ;; comment');
  expectResults(testEval, '(read-string "7 ;; comment")', '7');

  // ;;; TODO: really a step5 test
  // ;;
  // ;; Testing that (do (do)) not broken by TCO
  expectResults(testEval, '(do (do 1 2))', '2');

  // ;;
  // ;; Testing read-string, eval and slurp
  expectResults(testEval, '(read-string "(1 2 (3 4) nil)")', '(1 2 (3 4) nil)');

  expectResults(testEval, '(read-string "(+ 2 3)")', '(+ 2 3)');

  expectResults(testEval, '(read-string "7 ;; comment")', '7');

  // ;;; Differing output, but make sure no fatal error
  expectResults(testEval, '(read-string ";; comment")', "");


  expectResults(testEval, '(eval (read-string "(+ 2 3)"))', '5');

  // ;;; TODO: fix newline matching so that this works
  // ;;;(slurp "../tests/test.txt")
  // ;;;;=>"A line of text\n"


  // ;; Testing load-file

  expectResults(testEval, '(load-file "../../tests/inc.mal")', "MalFunction"); // TODO(adam): this also has that strange parsing bug
  expectResults(testEval, '(inc1 7)', '8');
  expectResults(testEval, '(inc2 7)', '9');
  expectResults(testEval, '(inc3 9)', '12');

  // ;;
  // ;; Testing that *ARGV* exists and is an empty list
  expectResults(testEval, '(list? *ARGV*)', 'true');
  expectResults(testEval, '*ARGV*', '()');

  // ;;
  // ;; -------- Optional Functionality --------

  // ;; Testing comments in a file
  expectResults(testEval, '(load-file "../../tests/incB.mal")', 'incB.mal return string');
  //; "incB.mal finished"
  //;=>"incB.mal return string"
  expectResults(testEval, '(inc4 7)', '11');
  expectResults(testEval, '(inc5 7)', '12');

// ;; Testing map literal across multiple lines in a file
  expectResults(testEval, '(load-file "../../tests/incC.mal")', 'incC.mal return string');
  // TODO(adam): fix up printing, this should of been '{"a" 1}'.
  expectResults(testEval, 'mymap', '{"a" 1}');

  // ;;; TODO: really a step5 test
  // ;; Testing that vector params not broken by TCO
  expectResults(testEval, '(def! g (fn* [] 78))', "MalFunction");
  expectResults(testEval, '(g)', '78');
  expectResults(testEval, '(def! g (fn* [a] (+ a 78)))', "MalFunction");
  expectResults(testEval, '(g 3)', '81');

}
