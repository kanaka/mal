library step7_quote_test;

import "../step7_quote.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), repl_env);

void main() {
  init();

  // ;; Testing cons function
  expectResults(testEval, '(cons 1 (list))', '(1)');
  expectResults(testEval, '(cons 1 (list 2))', '(1 2)');
  expectResults(testEval, '(cons 1 (list 2 3))', '(1 2 3)');
  expectResults(testEval, '(cons (list 1) (list 2 3))', '((1) 2 3)');

  // ;; Testing concat function
  expectResults(testEval, '(concat)', '()');
  expectResults(testEval, '(concat (list 1 2))', '(1 2)');
  expectResults(testEval, '(concat (list 1 2) (list 3 4))', '(1 2 3 4)');
  expectResults(testEval, '(concat (list 1 2) (list 3 4) (list 5 6))', '(1 2 3 4 5 6)');
  expectResults(testEval, '(concat (concat))', '()');


  // ;; Testing regular quote
  expectResults(testEval, """(quote 7)""", '7');
  expectResults(testEval, """'7""", '7');
  expectResults(testEval, """(quote (1 2 3))""", '(1 2 3)');
  expectResults(testEval, """'(1 2 3)""", '(1 2 3)');
  expectResults(testEval, """(quote (1 2 (3 4)))""", '(1 2 (3 4))');
  expectResults(testEval, """'(1 2 (3 4))""", '(1 2 (3 4))');


  // ;; Testing simple quasiquote
  expectResults(testEval, """(quasiquote 7)""", '7');
  expectResults(testEval, """`7""", '7');
  expectResults(testEval, """(quasiquote (1 2 3))""", '(1 2 3)');
  expectResults(testEval, """`(1 2 3)""", '(1 2 3)');
  expectResults(testEval, """(quasiquote (1 2 (3 4)))""", '(1 2 (3 4))');
  expectResults(testEval, """`(1 2 (3 4))""", '(1 2 (3 4))');


  // ;; Testing unquote
  expectResults(testEval, """`~7""", '7');
  expectResults(testEval, """(def! a 8)""", '8');
  expectResults(testEval, """`a""", 'a');
  expectResults(testEval, """`~a""", '8');
  expectResults(testEval, """`(1 a 3)""", '(1 a 3)');
  expectResults(testEval, """`(1 ~a 3)""", '(1 8 3)');
  expectResults(testEval, """(def! b '(1 "b" "d"))""", '(1 "b" "d")', isFailing: true); // Not printing out quotes properly
  expectResults(testEval, """`(1 b 3)""", '(1 b 3)');
  expectResults(testEval, """`(1 ~b 3)""", '(1 (1 "b" "d") 3)', isFailing: true); // Not printing out quotes properly


  // ;; Testing splice-unquote
  expectResults(testEval, """(def! c '(1 "b" "d"))""", '(1 "b" "d")', isFailing: true); // Not printing out quotes properly
  expectResults(testEval, """`(1 c 3)""", '(1 c 3)');
  expectResults(testEval, """`(1 ~@c 3)""", '(1 1 "b" "d" 3)', isFailing: true); // Not printing out quotes properly


  // ;; Testing symbol equality
  expectResults(testEval, """(= 'abc 'abc)""", 'true');
  expectResults(testEval, """(= 'abc 'abcd)""", 'false');
  expectResults(testEval, """(= 'abc "abc")""", 'false');
  expectResults(testEval, """(= "abc" 'abc)""", 'false');

  // ;;;;; Test quine
  // ;;; TODO: needs expect line length fix
  // ;;;((fn* [q] (quasiquote ((unquote q) (quote (unquote q))))) (quote (fn* [q] (quasiquote ((unquote q) (quote (unquote q)))))))
  // ;;;=>((fn* [q] (quasiquote ((unquote q) (quote (unquote q))))) (quote (fn* [q] (quasiquote ((unquote q) (quote (unquote q)))))))

  // ;;
  // ;; -------- Optional Functionality --------

  // ;; Testing cons, concat, first, rest with vectors
  expectResults(testEval, """(cons [1] [2 3])""", '([1] 2 3)');
  expectResults(testEval, """(cons 1 [2 3])""", '(1 2 3)');
  expectResults(testEval, """(concat [1 2] (list 3 4) [5 6])""", '(1 2 3 4 5 6)');

  // ;; Testing unquote with vectors
  expectResults(testEval, """(def! a 8)""", '8');
  expectResults(testEval, """`[1 a 3]""", '(1 a 3)');
  // ;;; TODO: fix this
  // ;;;;=>[1 a 3]

  // ;; Testing splice-unquote with vectors
  expectResults(testEval, """(def! c '(1 "b" "d"))""", '(1 "b" "d")', isFailing: true); // Not printing out quotes properly
  expectResults(testEval, """`[1 ~@c 3]""", '(1 1 "b" "d" 3)', isFailing: true); // Not printing out quotes properly
  // ;;; TODO: fix this
  // ;;;;=>[1 1 "b" "d" 3]
}
