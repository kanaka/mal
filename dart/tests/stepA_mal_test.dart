library stepA_mal_test;

import "../stepA_mal.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), repl_env);

void main() {
  init();

  // ;;;
  // ;;; See IMPL/tests/stepA_mal.mal for implementation specific
  // ;;; interop tests.
  // ;;;


  // ;;
  // ;; Testing readline
  // expectResults(testEval, '(readline "mal-user> ")', /* "hello" */ '"\"hello\""');

  // ;;
  // ;; Testing *host-language*
  // ;;; each impl is different, but this should return false
  // ;;; rather than throwing an exception
  expectResults(testEval, '(= "something bogus" *host-language*)', 'false');


  // ;;
  // ;; ------- Optional Functionality ----------
  // ;; ------- (Needed for self-hosting) -------

  // ;;
  // ;; Testing metadata on functions

  // ;;
  // ;; Testing metadata on mal functions
  expectResults(testEval, '(meta (fn* (a) a))', 'nil');
  expectResults(testEval, '(meta (with-meta (fn* (a) a) {"b" 1}))', '{"b" 1}');
  expectResults(testEval, '(meta (with-meta (fn* (a) a) "abc"))', 'abc');
  expectResults(testEval, '(def! l-wm (with-meta (fn* (a) a) {"b" 2}))', 'MalFunction');
  expectResults(testEval, '(meta l-wm)', '{"b" 2}');
  expectResults(testEval, '(meta (with-meta l-wm {"new_meta" 123}))', '{"new_meta" 123}');
  expectResults(testEval, '(meta l-wm)', '{"b" 2}');
  expectResults(testEval, '(def! f-wm (with-meta (fn* [a] (+ 1 a)) {"abc" 1}))', 'MalFunction');
  expectResults(testEval, '(meta f-wm)', '{"abc" 1}');
  expectResults(testEval, '(meta (with-meta f-wm {"new_meta" 123}))', '{"new_meta" 123}');
  expectResults(testEval, '(meta f-wm)', '{"abc" 1}');
  expectResults(testEval, '(def! f-wm2 ^{"abc" 1} (fn* [a] (+ 1 a)))', 'MalFunction');
  expectResults(testEval, '(meta f-wm2)', '{"abc" 1}');

  // ;; Meta of native functions should return nil (not fail)
  expectResults(testEval, '(meta +)', 'nil');


  // ;;
  // ;; Make sure closures and metadata co-exist
  expectResults(testEval, '(def! gen-plusX (fn* (x) (with-meta (fn* (b) (+ x b)) {"meta" 1})))', 'MalFunction');
  expectResults(testEval, '(def! plus7 (gen-plusX 7))', 'MalFunction');
  expectResults(testEval, '(def! plus8 (gen-plusX 8))', 'MalFunction');
  expectResults(testEval, '(plus7 8)', '15');
  expectResults(testEval, '(meta plus7)', '{"meta" 1}');
  expectResults(testEval, '(meta plus8)', '{"meta" 1}');
  expectResults(testEval, '(meta (with-meta plus7 {"meta" 2}))', '{"meta" 2}');
  expectResults(testEval, '(meta plus8)', '{"meta" 1}');


  // ;;
  // ;; Testing atoms
  expectResults(testEval, '(def! inc3 (fn* (a) (+ 3 a)))', 'MalFunction');
  expectResults(testEval, '(def! a (atom 2))','(atom 2)');

  // ;;;(type a)
  // ;;;;=>"atom"
  expectResults(testEval, '(deref a)', '2');
  expectResults(testEval, '@a', '2');
  expectResults(testEval, '(reset! a 3)', '3');
  expectResults(testEval, '@a', '3');
  expectResults(testEval, '(swap! a inc3)', '6');
  expectResults(testEval, '@a', '6');
  expectResults(testEval, '(swap! a (fn* (a) a))', '6');
  expectResults(testEval, '(swap! a (fn* (a) (* 2 a)))', '12');
  expectResults(testEval, '(swap! a (fn* (a b) (* a b)) 10)', '120');
  expectResults(testEval, '(swap! a + 3)', '123');

  // ;; Testing swap!/closure interaction
  expectResults(testEval, '(def! inc-it (fn* (a) (+ 1 a)))', 'MalFunction');
  expectResults(testEval, '(def! atm (atom 7))', '(atom 7)');
  expectResults(testEval, '(def! f (fn* [] (swap! atm inc-it)))', 'MalFunction');
  expectResults(testEval, '(f)', '8');
  expectResults(testEval, '(f)', '9');

  // ;; Testing hash-map evaluation and atoms (i.e. an env)
  expectResults(testEval, '(def! e (atom {"+" +}))', '(atom {"+" MalFunction})');
  expectResults(testEval, '(swap! e assoc "-" -)', '{"+" MalFunction"-" MalFunction}');
  expectResults(testEval, '( (get @e "+") 7 8)', '15');
  expectResults(testEval, '( (get @e "-") 11 8)', '3');


  // ;;
  // ;; ------- Optional Functionality --------------
  // ;; ------- (Not needed for self-hosting) -------

  // ;;
  // ;; Testing conj function
  expectResults(testEval, '(conj (list) 1)', '(1)');
  expectResults(testEval, '(conj (list 1) 2)', '(2 1)');
  expectResults(testEval, '(conj (list 2 3) 4)', '(4 2 3)');
  expectResults(testEval, '(conj (list 2 3) 4 5 6)', '(6 5 4 2 3)');
  expectResults(testEval, '(conj (list 1) (list 2 3))', '((2 3) 1)');
  expectResults(testEval, '(conj [] 1)', '[1]');
  expectResults(testEval, '(conj [1] 2)', '[1 2]');
  expectResults(testEval, '(conj [2 3] 4)', '[2 3 4]');
  expectResults(testEval, '(conj [2 3] 4 5 6)', '[2 3 4 5 6]');
  expectResults(testEval, '(conj [1] [2 3])', '[1 [2 3]]');


  // ;;
  // ;; Testing metadata on collections
  expectResults(testEval, '(meta [1 2 3])', 'nil');
  expectResults(testEval, '(with-meta [1 2 3] {"a" 1})', '[1 2 3]');
  expectResults(testEval, '(meta (with-meta [1 2 3] {"a" 1}))', '{"a" 1}');
  expectResults(testEval, '(vector? (with-meta [1 2 3] {"a" 1}))', 'true');
  expectResults(testEval, '(meta (with-meta [1 2 3] "abc"))', 'abc');
  expectResults(testEval, '(meta (with-meta (list 1 2 3) {"a" 1}))', '{"a" 1}');
  expectResults(testEval, '(list? (with-meta (list 1 2 3) {"a" 1}))', 'true');
  expectResults(testEval, '(meta (with-meta {"abc" 123} {"a" 1}))', '{"a" 1}');
  expectResults(testEval, '(map? (with-meta {"abc" 123} {"a" 1}))', 'true');

  // ;;; Not actually supported by Clojure
  // ;;;(meta (with-meta (atom 7) {"a" 1}))
  // ;;;;=>{"a" 1}
  expectResults(testEval, '(def! l-wm (with-meta [4 5 6] {"b" 2}))', '[4 5 6]');
  expectResults(testEval, '(meta l-wm)', '{"b" 2}');
  expectResults(testEval, '(meta (with-meta l-wm {"new_meta" 123}))', '{"new_meta" 123}');
  expectResults(testEval, '(meta l-wm)', '{"b" 2}');

  // ;;
  // ;; Testing metadata on builtin functions
  expectResults(testEval, '(meta +)', 'nil');
  expectResults(testEval, '(def! f-wm3 ^{"def" 2} +)', 'MalFunction');
  expectResults(testEval, '(meta f-wm3)', '{"def" 2}');
  expectResults(testEval, '(meta +)', 'nil');

}
