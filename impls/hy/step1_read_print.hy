#!/usr/bin/env hy

(import sys traceback)
(import [reader [read-str Blank]])
(import [printer [pr-str]])

(defn READ [str]
  (read-str str))

(defn EVAL [ast env]
  ast)

(defn PRINT [exp]
  (pr-str exp True))

(defn REP [str]
  (PRINT (EVAL (READ str) {})))

(defmain [&rest args]
  ;; indented to match later steps
      (while True
        (try
          (do (setv line (raw_input "user> "))
              (if (= "" line) (continue))
              (print (REP line)))
          (except [EOFError] (break))
          (except [Blank])
          (except [e Exception]
            (print (.join "" (apply traceback.format_exception
                                    (.exc_info sys))))))))
