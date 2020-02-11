#!/usr/bin/env hy

(import [hy.models [HySymbol :as Sym]])
(import sys traceback)
(import [mal_types [MalException]])
(import [reader [read-str Blank]])
(import [printer [pr-str]])
(import [env [env-new env-get env-set]])
(import core)

;; read
(defn READ [str]
  (read-str str))

;; eval
(defn eval-ast [ast env]
  ;;(print "eval-ast:" ast (type ast))
  (if
    (symbol? ast)         (env-get env ast)
    (instance? dict ast)  (dict (map (fn [k]
                                       [(EVAL k env) (EVAL (get ast k) env)])
                                     ast))
    (instance? tuple ast) (tuple (map (fn [x] (EVAL x env)) ast))
    (instance? list ast)  (list (map (fn [x] (EVAL x env)) ast))
    True                  ast))

(defn EVAL [ast env]
  ;;(print "EVAL:" ast (type ast))
  ;; indented to match later steps
      (if (not (instance? tuple ast))
        (eval-ast ast env)

        ;; apply list
            (do
              (setv [a0 a1 a2] [(nth ast 0) (nth ast 1) (nth ast 2)])
              (if
                (none? a0)
                ast

                (= (Sym "def!") a0)
                (env-set env a1 (EVAL a2 env))

                (= (Sym "let*") a0)
                (do
                  (setv env (env-new env))
                  (for [[b e] (partition a1 2)]
                    (env-set env b (EVAL e env)))
                  (EVAL a2 env))

                (= (Sym "do") a0)
                (last (eval-ast (list (rest ast)) env))

                (= (Sym "if") a0)
                (do
                  (setv cond (EVAL a1 env))
                  (if (or (none? cond) (and (instance? bool cond)
                                            (= cond False)))
                    (if (> (len ast) 2)
                      (EVAL (nth ast 3) env)
                      None)
                    (EVAL a2 env)))

                (= (Sym "fn*") a0)
                (fn [&rest args]
                  (EVAL a2 (env-new env a1 (or args []))))

                ;; apply
                (do
                  (setv el (eval-ast ast env)
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

;; core.hy: defined using Hy
(for [k core.ns]
  (env-set repl-env (Sym k) (get core.ns k)))

;; core.mal: defined using the language itself
(REP "(def! not (fn* [a] (if a false true)))")

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
            (setv msg (.rstrip (.join "" (apply traceback.format_exception
                                                (.exc_info sys)))))
            (if (instance? MalException e)
              (setv msg (+ (.rstrip msg) ": " (pr-str e.val True))))
            (print msg)))))
