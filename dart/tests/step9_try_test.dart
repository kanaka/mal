library step9_try_test;

import "../step9_try.dart";
import "test_helpers.dart";

Function testEval = (String input) => EVAL(READ(input), repl_env);

void main() {
  init();

  // DEBUG
  expectResults(testEval, '(try* (map throw [7]) (catch* exc exc))', '7');
  expectResults(testEval, '(get (assoc (assoc {"a" 1 } "b" 2) "c" 3) "a")', '1');
  expectResults(testEval, '(vector 3 4 5)', '[3 4 5]');


  // ;;
  // ;; Testing try*/catch*
  expectResults(testEval, '(try* 123 (catch* e 456))', '123');
  expectResults(testEval, '(try* (abc 1 2) (catch* exc (prn "exc is:" exc)))', /*; "exc is:" "'abc' not found"*/ 'nil');

  // ;;;TODO: fix so long lines don't trigger ANSI escape codes ;;;(try*
  // ;;;(try* (throw ["data" "foo"]) (catch* exc (do (prn "exc is:" exc) 7))) ;;;;
  // ;;;; "exc is:" ["data" "foo"] ;;;;=>7
  // ;;;;=>7
  expectResults(testEval, '(try* (throw (list 1 2 3)) (catch* exc (do (prn "err:" exc) 7)))', /* ; "err:" (1 2 3) */ '7');
  expectResults(testEval, '(try* (throw "my exception") (catch* exc (do (prn "exc:" exc) 7)))', /* ; "exc:" "my exception" */ '7');

  // ;;; Test that throw is a function:
  expectResults(testEval, '(try* (map throw [7]) (catch* exc exc))', '7');

  // ;;
  // ;; Testing builtin functions
  expectResults(testEval, "(symbol? 'abc)", 'true');
  expectResults(testEval, '(symbol? "abc")', 'false');
  expectResults(testEval, '(nil? nil)', 'true');
  expectResults(testEval, '(nil? true)', 'false');
  expectResults(testEval, '(true? true)', 'true');
  expectResults(testEval, '(true? false)', 'false');
  expectResults(testEval, '(true? true?)', 'false');
  expectResults(testEval, '(false? false)', 'true');
  expectResults(testEval, '(false? true)', 'false');

  // ;; Testing apply function with core functions
  expectResults(testEval, '(apply + (list 2 3))', '5');
  expectResults(testEval, '(apply + 4 (list 5))', '9');
  expectResults(testEval, '(apply prn (list 1 2 "3" (list)))', 'nil');
  //  ; 1 2 "3" ()
  expectResults(testEval, '(apply prn 1 2 (list "3" (list)))', 'nil');
  //  ; 1 2 "3" ()

  // ;; Testing apply function with user functions
  expectResults(testEval, '(apply (fn* (a b) (+ a b)) (list 2 3))', '5');
  expectResults(testEval, '(apply (fn* (a b) (+ a b)) 4 (list 5))', '9');

  // ;; Testing map function
  expectResults(testEval, '(def! nums (list 1 2 3))', '(1 2 3)');
  expectResults(testEval, '(def! double (fn* (a) (* 2 a)))', 'MalFunction');
  expectResults(testEval, '(double 3)', '6');
  expectResults(testEval, '(map double nums) ', '(2 4 6)');
  expectResults(testEval, '(map (fn* (x) (symbol? x)) (list 1 (symbol "two") "three"))', '(false true false)');

  // ;;
  // ;; ------- Optional Functionality ----------
  // ;; ------- (Needed for self-hosting) -------

  // ;; Testing symbol and keyword functions
  expectResults(testEval, '(symbol? :abc)', 'false');
  expectResults(testEval, "(symbol? 'abc)", 'true');
  expectResults(testEval, '(symbol? "abc")', 'false');
  expectResults(testEval, '(symbol? (symbol "abc"))', 'true');
  expectResults(testEval, '(keyword? :abc)', 'true');
  expectResults(testEval, "(keyword? 'abc)", 'false');
  expectResults(testEval, '(keyword? "abc")', 'false');
  expectResults(testEval, '(keyword? (keyword "abc"))', 'true');
  expectResults(testEval, '(symbol "abc")', 'abc');
  // ;;;TODO: all implementations should suppport this too
  // ;;;(keyword :abc)
  // ;;;;=>:abc
  expectResults(testEval, '(keyword "abc")', ':abc');

  // ;; Testing sequential? function
  expectResults(testEval, '(sequential? (list 1 2 3))', 'true');
  expectResults(testEval, '(sequential? [15])', 'true');
  expectResults(testEval, '(sequential? sequential?)', 'false');
  expectResults(testEval, '(sequential? nil)', 'false');
  expectResults(testEval, '(sequential? "abc")', 'false');

  // ;; Testing apply function with core functions and arguments in vector
  expectResults(testEval, '(apply + 4 [5])', '9');
  expectResults(testEval, '(apply prn 1 2 ["3" 4])', /* ; 1 2 "3" 4 */ 'nil');
  // ;; Testing apply function with user functions and arguments in vector
  expectResults(testEval, '(apply (fn* (a b) (+ a b)) [2 3])', '5');
  expectResults(testEval, '(apply (fn* (a b) (+ a b)) 4 [5])', '9');


  // ;; Testing map function with vectors
  expectResults(testEval, '(map (fn* (a) (* 2 a)) [1 2 3])', '(2 4 6)');

  // ;; Testing vector functions
  expectResults(testEval, '(vector? [10 11])', 'true');
  expectResults(testEval, "(vector? '(12 13))", 'false');
  expectResults(testEval, '(vector 3 4 5)', '[3 4 5]');
  expectResults(testEval, '(map? {})', 'true');
  expectResults(testEval, "(map? '())", 'false');
  expectResults(testEval, '(map? [])', 'false');
  expectResults(testEval, "(map? 'abc)", 'false');
  expectResults(testEval, '(map? :abc)', 'false');

  // ;;
  // ;; Testing hash-maps
  expectResults(testEval, '(hash-map "a" 1)', '{"a" 1}');
  expectResults(testEval, '{"a" 1}', '{"a" 1}');
  expectResults(testEval, '(assoc {} "a" 1)', '{"a" 1}');
  expectResults(testEval, '(get (assoc (assoc {"a" 1 } "b" 2) "c" 3) "a")', '1');
  expectResults(testEval, '(def! hm1 (hash-map))', '{}');
  expectResults(testEval, '(map? hm1)', 'true');
  expectResults(testEval, '(map? 1)', 'false');
  expectResults(testEval, '(map? "abc")', 'false');
  expectResults(testEval, '(get nil "a")', 'nil');
  expectResults(testEval, '(get hm1 "a")', 'nil');
  expectResults(testEval, '(contains? hm1 "a")', 'false');
  expectResults(testEval, '(def! hm2 (assoc hm1 "a" 1))', '{"a" 1}');
  expectResults(testEval, '(get hm1 "a")', 'nil');
  expectResults(testEval, '(contains? hm1 "a")', 'false');
  expectResults(testEval, '(get hm2 "a")', '1');
  expectResults(testEval, '(contains? hm2 "a")', 'true');


  // ;;; TODO: fix. Clojure returns nil but this breaks mal impl
  expectResults(testEval, '(keys hm1)', '()');
  expectResults(testEval, '(keys hm2)', '(a)');

  // ;;; TODO: fix. Clojure returns nil but this breaks mal impl
  expectResults(testEval, '(vals hm1)', '()');
  expectResults(testEval, '(vals hm2)', '(1)');
  expectResults(testEval, '(count (keys (assoc hm2 "b" 2 "c" 3)))', '3');
  expectResults(testEval, '(def! hm3 (assoc hm2 "b" 2))', '{"a" 1"b" 2}');
  expectResults(testEval, '(count (keys hm3))', '2');
  expectResults(testEval, '(count (vals hm3))', '2');
  expectResults(testEval, '(dissoc hm3 "a")', '{"b" 2}');
  expectResults(testEval, '(dissoc hm3 "a" "b")', '{}');
  expectResults(testEval, '(dissoc hm3 "a" "b" "c")', '{}');
  expectResults(testEval, '(count (keys hm3))', '2');

  // ;; Testing keywords as hash-map keys
  expectResults(testEval, '(get {:abc 123} :abc)', '123');
  expectResults(testEval, '(contains? {:abc 123} :abc)', 'true');
  expectResults(testEval, '(contains? {:abcd 123} :abc)', 'false');
  expectResults(testEval, '(assoc {} :bcd 234)', '{:bcd 234}');
  expectResults(testEval, '(dissoc {:cde 345 :fgh 456} :cde)', '{:fgh 456}');
  expectResults(testEval, '(keyword? (nth (keys {:abc 123 :def 456}) 0))', 'true');
  // ;;; TODO: support : in strings in make impl
  // ;;;(keyword? (nth (keys {":abc" 123 ":def" 456}) 0))
  // ;;;;=>false
  expectResults(testEval, '(keyword? (nth (vals {"a" :abc "b" :def}) 0))', 'true');
}
