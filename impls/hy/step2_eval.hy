#!/usr/bin/env hy

(import sys traceback)
(import [reader [read-str Blank]])
(import [printer [pr-str]])

;; read
(defn READ [str]
  (read-str str))

;; eval
(defn EVAL [ast env]
  ;; indented to match later steps
      (if
        (symbol? ast)
        (if (.has_key env ast) (get env ast)
          (raise (Exception (+ ast " not found"))))

        (instance? dict ast)
        (dict (map (fn [k]
                     [k (EVAL (get ast k) env)])
                   ast))

        (instance? list ast)
        (list (map (fn [x] (EVAL x env)) ast))

        (not (instance? tuple ast))
        ast

        (empty? ast)
        ast

        ;; apply list
                ;; apply
                (do
                  (setv el (list (map (fn [x] (EVAL x env)) ast))
                        f (first el)
                        args (list (rest el)))
                  (apply f args))))

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
