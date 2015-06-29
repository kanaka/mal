library step4_if_fn_do_test;

import "../step4_if_fn_do.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), repl_env);

void main() {
  init();

// ;; -----------------------------------------------------
  // ;; Testing list functions
  expectResults(testEval, '(list)', '()');
  expectResults(testEval, '(list? (list))', 'true');
  expectResults(testEval, '(empty? (list))', 'true');
  expectResults(testEval, '(empty? (list 1))', 'false');
  expectResults(testEval, '(list 1 2 3)', '(1 2 3)');
  expectResults(testEval, '(count (list 1 2 3))', '3');
  expectResults(testEval, '(count (list))', '0');
  expectResults(testEval, '(count nil)', '0');
  expectResults(testEval, '(if (> (count (list 1 2 3)) 3) "yes" "no")', 'no');
  expectResults(testEval, '(if (>= (count (list 1 2 3)) 3) "yes" "no")', 'yes');
  // ;; Testing if form
  expectResults(testEval, '(if true 7 8)', '7');
  expectResults(testEval, '(if false 7 8)', '8');
  expectResults(testEval, '(if true (+ 1 7) (+ 1 8))', '8');
  expectResults(testEval, '(if false (+ 1 7) (+ 1 8))', '9');
  expectResults(testEval, '(if nil 7 8)', '8');
  expectResults(testEval, '(if 0 7 8)', '7');
  expectResults(testEval, '(if "" 7 8)', '7');
  expectResults(testEval, '(if (list) 7 8)', '7');
  expectResults(testEval, '(if (list 1 2 3) 7 8)', '7');
  expectResults(testEval, '(= (list) nil)', 'false');
  // ;; Testing 1-way if form
  expectResults(testEval, '(if false (+ 1 7))', 'nil');
  expectResults(testEval, '(if nil 8 7)', '7');
  expectResults(testEval, '(if true (+ 1 7))', '8');
  // ;; Testing basic conditionals
  expectResults(testEval, '(= 2 1)', 'false');
  expectResults(testEval, '(= 1 1)', 'true');
  expectResults(testEval, '(= 1 2)', 'false');
  expectResults(testEval, '(= 1 (+ 1 1))', 'false');
  expectResults(testEval, '(= 2 (+ 1 1))', 'true');
  expectResults(testEval, '(= nil 1)', 'false');
  expectResults(testEval, '(= nil nil)', 'true');
  expectResults(testEval, '(> 2 1)', 'true');
  expectResults(testEval, '(> 1 1)', 'false');
  expectResults(testEval, '(> 1 2)', 'false');
  expectResults(testEval, '(>= 2 1)', 'true');
  expectResults(testEval, '(>= 1 1)', 'true');
  expectResults(testEval, '(>= 1 2)', 'false');
  expectResults(testEval, '(< 2 1)', 'false');
  expectResults(testEval, '(< 1 1)', 'false');
  expectResults(testEval, '(< 1 2)', 'true');
  expectResults(testEval, '(<= 2 1)', 'false');
  expectResults(testEval, '(<= 1 1)', 'true');
  expectResults(testEval, '(<= 1 2)', 'true');
  // ;; Testing equality
  expectResults(testEval, '(= 1 1)', 'true');
  expectResults(testEval, '(= 0 0)', 'true');
  expectResults(testEval, '(= 1 0)', 'false');
  expectResults(testEval, '(= "" "")', 'true');
  expectResults(testEval, '(= "abc" "")', 'false');
  expectResults(testEval, '(= "" "abc")', 'false');
  expectResults(testEval, '(= "abc" "def")', 'false');
  expectResults(testEval, '(= (list) (list))', 'true');
  expectResults(testEval, '(= (list 1 2) (list 1 2))', 'true');
  expectResults(testEval, '(= (list 1) (list))', 'false');
  expectResults(testEval, '(= (list) (list 1))', 'false');
  expectResults(testEval, '(= 0 (list))', 'false');
  expectResults(testEval, '(= (list) 0)', 'false');
  expectResults(testEval, '(= (list) "")', 'false');
  expectResults(testEval, '(= "" (list))', 'false');
  // ;; Testing builtin and user defined functions
  expectResults(testEval, '(+ 1 2)', '3');
  expectResults(testEval, '( (fn* (a b) (+ b a)) 3 4)', '7');
  expectResults(testEval, '( (fn* () 4) )', '4');
  expectResults(testEval, '( (fn* (f x) (f x)) (fn* (a) (+ 1 a)) 7)', '8');
  // ;; Testing closures
  expectResults(testEval, '( ( (fn* (a) (fn* (b) (+ a b))) 5) 7)', '12');
  expectResults(testEval, '(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))', "MalFunction");
  expectResults(testEval, '(def! plus5 (gen-plus5))', "MalFunction");
  expectResults(testEval, '(plus5 7)', '12');
  expectResults(testEval, '(def! gen-plusX (fn* (x) (fn* (b) (+ x b))))', "MalFunction");
  expectResults(testEval, '(def! plus7 (gen-plusX 7))', "MalFunction");
  expectResults(testEval, '(plus7 8)', '15');
  // ;; Testing variable length arguments
  expectResults(testEval, '( (fn* (& more) (count more)) 1 2 3)', '3');
  expectResults(testEval, '( (fn* (& more) (count more)) 1)', '1');
  expectResults(testEval, '( (fn* (& more) (count more)) )', '0');
  expectResults(testEval, '( (fn* (a & more) (count more)) 1 2 3)', '2');
  expectResults(testEval, '( (fn* (a & more) (count more)) 1)', '0');
  // ;; Testing language defined not function
  expectResults(testEval, '(not false)', 'true');
  expectResults(testEval, '(not true)', 'false');
  expectResults(testEval, '(not "a")', 'false');
  expectResults(testEval, '(not 0)', 'false');
  // ;; Testing do form
  expectResults(testEval, '(do (prn "prn output1"))',
  // ; "prn output1"
  'nil');
  expectResults(testEval, '(do (prn "prn output2") 7)',
  // ; "prn output2"
  '7');
  expectResults(testEval, '(do (prn "prn output1") (prn "prn output2") (+ 1 2))',
  // ; "prn output1"
  // ; "prn output2"
  '3');
  expectResults(testEval, '(do (def! a 6) 7 (+ a 8))', '14');
  expectResults(testEval, 'a', '6');
  // ;; Testing recursive sumdown function
  expectResults(testEval, '(def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown  (- N 1))) 0)))', "MalFunction");
  expectResults(testEval, '(sumdown 1)', '1');
  expectResults(testEval, '(sumdown 2)', '3');
  expectResults(testEval, '(sumdown 6)', '21');
  // ;; Testing recursive fibonacci function
  expectResults(testEval, '(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))', "MalFunction");
  expectResults(testEval, '(fib 1)', '1');
  expectResults(testEval, '(fib 2)', '2');
  expectResults(testEval, '(fib 4)', '5');
  expectResults(testEval, '(fib 10)', '89');
  // ;; -----------------------------------------------------
  // ;; Testing string quoting
  expectResults(testEval, '""', '');
  expectResults(testEval, '"abc"', 'abc');
  expectResults(testEval, '"abc  def"', 'abc  def');
  expectResults(testEval, "\"", "\"", isFailing: true); // TODO(adam): tokenizer fails to create token for single"

  // ;; Testing pr-str
  expectResults(testEval, '(pr-str)', "");
  expectResults(testEval, '(pr-str "")', "\"\"");
  expectResults(testEval, '(pr-str "abc")', "\"abc\"");
  expectResults(testEval, '(pr-str "abc  def" "ghi jkl")', "\"abc  def\" \"ghi jkl\"");
  expectResults(testEval, '(pr-str "\"")', '"', isFailing: true); // TODO(adam): unsure about proper pr-str behavior.
  expectResults(testEval, '(pr-str (list 1 2 "abc" "\"") "def")', "(1 2 \"abc\" \"\\\"\") \"def\"", isFailing: true); // TODO(adam): this does not lex properly
  // ;; Testing str
  expectResults(testEval, '(str)', "");
  expectResults(testEval, '(str "")', '');
  expectResults(testEval, '(str "abc")', 'abc');
  expectResults(testEval, '(str "\"")', '\"', isFailing: true);
  expectResults(testEval, '(str 1 "abc" 3)', "1abc3", isFailing: true);
  expectResults(testEval, '(str "abc  def" "ghi jkl")', '"abc  defghi jkl"', isFailing: true);
  // ;;; TODO: get this working properly
  // ;;;(str (list 1 2 "abc" "\"") "def")
  // ;;;;=>"(1 2 \"abc\" \"\\\"\")def"
  // ;; Testing prn
  expectResults(testEval, '(prn)',
  // ;
  'nil');
  expectResults(testEval, '(prn "")',
  // ; ""
  'nil');
  expectResults(testEval, '(prn "abc")',
  // ; "abc"
  'nil');
  expectResults(testEval, '(prn "abc  def" "ghi jkl")', '', isFailing: true); // ignore this test output
  // ; "abc  def" "ghi jkl"
  expectResults(testEval, '(prn "\"")',
  // ; "\""
  'nil');
  expectResults(testEval, '(prn (list 1 2 "abc" "\"") "def")',
  // ; (1 2 "abc" "\"") "def"
  'nil', isFailing: true);
  // ;; Testing println
  expectResults(testEval, '(println)',
  // ;
  'nil');
  expectResults(testEval, '(println "")',
  // ;
  'nil');
  expectResults(testEval, '(println "abc")',
  // ; abc
  'nil');
  expectResults(testEval, '(println "abc  def" "ghi jkl")', '', isFailing: true); // ignore this test output
  // ; abc  def ghi jkl
  expectResults(testEval, '(println "\"")',
  // ; "
  'nil');
  expectResults(testEval, '(println (list 1 2 "abc" "\"") "def")',
  // ; (1 2 abc ") def
  'nil', isFailing: true);

  // ;;
  // ;; -------- Optional Functionality --------
  // ;; Testing keywords
  expectResults(testEval, '(= :abc :abc)', 'true');
  expectResults(testEval, '(= :abc :def)', 'false');
  expectResults(testEval, '(= :abc ":abc")', 'false');
  // ;; Testing vector truthiness
  expectResults(testEval, '(if [] 7 8)', '7');
  // ;; Testing vector functions
  expectResults(testEval, '(count [1 2 3])', '3');
  expectResults(testEval, '(empty? [1 2 3])', 'false');
  expectResults(testEval, '(empty? [])', 'true');

  expectResults(testEval, '(list? [4 5 6])', 'false');
  // ;; Testing vector equality
  expectResults(testEval, '(= [] (list))', 'true');
  expectResults(testEval, '(= (list 1 2) [1 2])', 'true');
  expectResults(testEval, '(= (list 1) [])', 'false');
  expectResults(testEval, '(= [] [1])', 'false');
  expectResults(testEval, '(= 0 [])', 'false');
  expectResults(testEval, '(= [] 0)', 'false');
  expectResults(testEval, '(= [] "")', 'false');
  expectResults(testEval, '(= "" [])', 'false');
  // ;; Testing vector parameter lists
  expectResults(testEval, '( (fn* [] 4) )', '4');
  expectResults(testEval, '( (fn* [f x] (f x)) (fn* [a] (+ 1 a)) 7)', '8');
}
