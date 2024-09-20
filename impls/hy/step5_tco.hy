#!/usr/bin/env hy

(import [hy.models [HySymbol :as Sym]])
(import sys traceback)
(import [mal_types [MalException]])
(import [reader [read-str Blank]])
(import [printer [pr-str]])
(import [env [env-new env-get env-set env-find]])
(import core)

;; read
(defn READ [str]
  (read-str str))

;; eval
(defn EVAL [ast env]
  (setv res None)
  (while True
    (setv [dbgevalenv] [(env-find env (Sym "DEBUG-EVAL"))])
    (if dbgevalenv
      (do (setv [dbgevalsym] [(env-get dbgevalenv (Sym "DEBUG-EVAL"))])
          (if (not (none? dbgevalsym))
            (print "EVAL:" (pr-str ast True)))))
    (setv res
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
                  (setv ast a2)
                  (continue)) ;; TCO

                (= (Sym "do") a0)
                (do (list (map (fn [x] (EVAL x env))
                               (list (butlast (rest ast)))))
                    (setv ast (last ast))
                    (continue)) ;; TCO

                (= (Sym "if") a0)
                (do
                  (setv cond (EVAL a1 env))
                  (if (or (none? cond) (and (instance? bool cond)
                                            (= cond False)))
                    (if (> (len ast) 2)
                      (do (setv ast (nth ast 3)) (continue)) ;; TCO
                      None)
                    (do (setv ast a2) (continue)))) ;; TCO

                (= (Sym "fn*") a0)
                (do
                  (setv func (fn [&rest args]
                               (EVAL a2 (env-new env a1 (or args []))))
                        func.ast a2
                        func.env env
                        func.params a1)
                  func)

                ;; apply
                (do
                  (setv el (list (map (fn [x] (EVAL x env)) ast))
                        f (first el)
                        args (list (rest el)))
                  (if (hasattr f "ast")
                    (do (setv ast f.ast
                              env (env-new f.env f.params args))
                        (continue)) ;; TCO
                    (apply f args)))))))
    (break))
  res)

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
