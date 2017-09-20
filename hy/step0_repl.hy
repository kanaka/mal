#!/usr/bin/env hy

(defn READ [str]
  str)

(defn EVAL [ast env]
  ast)

(defn PRINT [exp]
 exp)

(defn REP [str]
  (PRINT (EVAL (READ str) {})))

(while True
  (try
    (do (setv line (raw_input "user> "))
        (if (= "" line) (continue))
        (print (REP line)))
    (except [EOFError] (break))))
