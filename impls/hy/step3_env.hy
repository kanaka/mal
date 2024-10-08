#!/usr/bin/env hy

(import [hy.models [HySymbol :as Sym]])
(import sys traceback)
(import [reader [read-str Blank]])
(import [printer [pr-str]])
(import [env [env-new env-get env-set env-find]])

;; read
(defn READ [str]
  (read-str str))

;; eval
(defn EVAL [ast env]
  ;; indented to match later steps
    (setv [dbgevalenv] [(env-find env (Sym "DEBUG-EVAL"))])
    (if dbgevalenv
      (do (setv [dbgevalsym] [(env-get dbgevalenv (Sym "DEBUG-EVAL"))])
          (if (not (none? dbgevalsym))
            (print "EVAL:" (pr-str ast True)))))
      (if
        (symbol? ast)
        (env-get env ast)

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
            (do
              (setv [a0 a1 a2] [(nth ast 0) (nth ast 1) (nth ast 2)])
              (if
                (= (Sym "def!") a0)
                (env-set env a1 (EVAL a2 env))

                (= (Sym "let*") a0)
                (do
                  (setv env (env-new env))
                  (for [[b e] (partition a1 2)]
                    (env-set env b (EVAL e env)))
                  (EVAL a2 env))

                ;; apply
                (do
                  (setv el (list (map (fn [x] (EVAL x env)) ast))
                        f (first el)
                        args (list (rest el)))
                  (apply f args))))))

;; print
(defn PRINT [exp]
  (pr-str exp True))

;; repl
(def repl-env (env-new))
(defn REP [str]
  (PRINT (EVAL (READ str) repl-env)))

(env-set repl-env '+ +)
(env-set repl-env '- -)
(env-set repl-env '* *)
(env-set repl-env '/ /)

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
