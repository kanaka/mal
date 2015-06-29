library step8_macros_test;

import "../step8_macros.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), repl_env);

void main() {
  init();

  // ;; Testing non-macro function
  expectResults(testEval, """(not (= 1 1))""",'false');
  // ;;; This should fail if it is a macro
  expectResults(testEval, """(not (= 1 2))""",'true');


  // ;; Testing trivial macros
  expectResults(testEval, """(defmacro! one (fn* () 1))""", "MalFunction");
  expectResults(testEval, """(one)""",'1');
  expectResults(testEval, """(defmacro! two (fn* () 2))""", "MalFunction");
  expectResults(testEval, """(two)""",'2');

  // ;; Testing unless macros
  expectResults(testEval, """(defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a)))""", "MalFunction");
  expectResults(testEval, """(unless false 7 8)""",'7');
  expectResults(testEval, """(unless true 7 8)""",'8');
  expectResults(testEval, """(defmacro! unless2 (fn* (pred a b) `(if (not ~pred) ~a ~b)))""", "MalFunction");
  expectResults(testEval, """(unless2 false 7 8)""",'7');
  expectResults(testEval, """(unless2 true 7 8)""",'8');

  // ;; Testing macroexpand
  expectResults(testEval, """(macroexpand (unless2 2 3 4))""",'(if (not 2) 3 4)');


  // ;; Testing nth, first and rest functions
  expectResults(testEval, """(nth '(1) 0)""",'1');
  expectResults(testEval, """(nth '(1 2) 1)""",'2');
  expectResults(testEval, """(def! x "x")""", "x");
  expectResults(testEval, """(def! x (nth '(1 2) 2))""", "nth: index out of range");
  expectResults(testEval, """x""",'x');
  expectResults(testEval, """(first '())""",'nil');
  expectResults(testEval, """(first '(6))""",'6');
  expectResults(testEval, """(first '(7 8 9))""",'7');
  expectResults(testEval, """(rest '())""",'()');
  expectResults(testEval, """(rest '(6))""",'()');
  expectResults(testEval, """(rest '(7 8 9))""",'(8 9)');


  // ;; Testing or macro
  expectResults(testEval, """(or)""",'nil');
  expectResults(testEval, """(or 1)""",'1');
  expectResults(testEval, """(or 1 2 3 4)""",'1');
  expectResults(testEval, """(or false 2)""",'2');
  expectResults(testEval, """(or false nil 3)""",'3');
  expectResults(testEval, """(or false nil false false nil 4)""",'4');
  expectResults(testEval, """(or false nil 3 false nil 4)""",'3');
  expectResults(testEval, """(or (or false 4))""",'4');

  // ;; Testing cond macro
  expectResults(testEval, """(cond)""",'nil');
  expectResults(testEval, """(cond true 7)""",'7');
  expectResults(testEval, """(cond true 7 true 8)""",'7');
  expectResults(testEval, """(cond false 7 true 8)""",'8');
  expectResults(testEval, """(cond false 7 false 8 "else" 9)""",'9');
  expectResults(testEval, """(cond false 7 (= 2 2) 8 "else" 9)""",'8');
  expectResults(testEval, """(cond false 7 false 8 false 9)""",'nil');

  // ;;
  // ;; Loading core.mal
  expectResults(testEval, """(load-file "../../core.mal")""", "MalFunction");

  // ;; Testing and macro
  expectResults(testEval, """(and)""",'true');
  expectResults(testEval, """(and 1)""",'1');
  expectResults(testEval, """(and 1 2)""",'2');
  expectResults(testEval, """(and 1 2 3)""",'3');
  expectResults(testEval, """(and 1 2 3 4)""",'4');
  expectResults(testEval, """(and 1 2 3 4 false)""",'false');
  expectResults(testEval, """(and 1 2 3 4 false 5)""",'false');

  // ;; Testing -> macro
  expectResults(testEval, """(-> 7)""",'7');
  expectResults(testEval, """(-> (list 7 8 9) first)""",'7');
  expectResults(testEval, """(-> (list 7 8 9) (first))""",'7');
  expectResults(testEval, """(-> (list 7 8 9) first (+ 7))""",'14');
  expectResults(testEval, """(-> (list 7 8 9) rest (rest) first (+ 7))""",'16');

  // ;; Testing EVAL in let*
  expectResults(testEval, """(let* (x (or nil "yes")) x)""",'yes');

  // ;;
  // ;; -------- Optional Functionality --------

  // ;; Testing nth, first, rest with vectors
  expectResults(testEval, """(nth [1] 0)""",'1');
  expectResults(testEval, """(nth [1 2] 1)""",'2');
  expectResults(testEval, """(def! x "x")""", "x");
  expectResults(testEval, """(def! x (nth [1 2] 2))""", "nth: index out of range");
  expectResults(testEval, """x""",'x');
  expectResults(testEval, """(first [])""",'nil');
  expectResults(testEval, """(first [10])""",'10');
  expectResults(testEval, """(first [10 11 12])""",'10');
  expectResults(testEval, """(rest [])""",'()');
  expectResults(testEval, """(rest [10])""",'()');
  expectResults(testEval, """(rest [10 11 12])""",'(11 12)');

  // ;; Testing EVAL in vector let*
  expectResults(testEval, """(let* [x (or nil "yes")] x)""",'yes');

}
