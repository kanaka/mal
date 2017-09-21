#!/usr/bin/env hy

(import [hy.models [HyDict :as Map]])
(import sys traceback)
(import [reader [read-str Blank]])
(import [printer [pr-str]])

;; read
(defn READ [str]
  (read-str str))

;; eval
(defn eval-ast [ast env]
  (if
    (symbol? ast)         (if (.has_key env ast) (get env ast)
                              (raise (Exception (+ ast " not found"))))
    (instance? Map ast)   (Map (map (fn [x] (EVAL x env)) ast))
    (instance? tuple ast) (tuple (map (fn [x] (EVAL x env)) ast))
    (instance? list ast)  (list (map (fn [x] (EVAL x env)) ast))
    True                  ast))

(defn EVAL [ast env]
  (if (not (instance? tuple ast))
    (eval-ast ast env)

    (if
      (empty? ast) ast
      (do
        (setv el (eval-ast ast env)
              f (first el)
              args (rest el))
        (apply f args)))))

;; print
(defn PRINT [exp]
  (pr-str exp True))

;; repl

(def repl-env {'+ +
               '- -
               '* *
               '/ (fn [a b] (int (/ a b)))})

(defn REP [str]
  (PRINT (EVAL (READ str) repl-env)))

(while True
  (try
    (do (setv line (raw_input "user> "))
        (if (= "" line) (continue))
        (print (REP line)))
    (except [EOFError] (break))
    (except [Blank])
    (except []
      (print (.join "" (apply traceback.format_exception (.exc_info sys)))))))
