#!/usr/bin/env hy

(import [hy.models [HyDict :as Map HyString :as Str HySymbol :as Sym]])
(import sys traceback)
(import [reader [read-str Blank]])
(import [printer [pr-str]])
(import [env [env-new env-get env-set]])
(import core)

;; read
(defn READ [str]
  (read-str str))

;; eval
(defn pair? [x]
  (and (coll? x) (> (len x) 0)))

(defn QUASIQUOTE [ast]
  (if
    (not (pair? ast))
    (tuple [(Sym "quote") ast])

    (= (Sym "unquote") (first ast))
    (nth ast 1)

    (and (pair? (first ast))
         (= (Sym "splice-unquote") (first (first ast))))
    (tuple [(Sym "concat") (nth (first ast) 1) (QUASIQUOTE (tuple (rest ast)))])

    True
    (tuple [(Sym "cons") (QUASIQUOTE (first ast)) (QUASIQUOTE (tuple (rest ast)))])))

(defn eval-ast [ast env]
  ;;(print "eval-ast:" ast (type ast))
  (if
    (symbol? ast)         (env-get env ast)
    (instance? Map ast)   (Map (map (fn [x] (EVAL x env)) ast))
    (instance? tuple ast) (tuple (map (fn [x] (EVAL x env)) ast))
    (instance? list ast)  (list (map (fn [x] (EVAL x env)) ast))
    True                  ast))

(defn EVAL [ast env]
  ;;(print "EVAL:" ast (type ast) (instance? tuple ast))
  (setv res None)
  (while True
    (if (not (instance? tuple ast))
      (setv res (eval-ast ast env))
  
      ;; apply list
      (do
        (setv [a0 a1 a2] [(nth ast 0) (nth ast 1) (nth ast 2)])
        (if
          (none? a0)
          (setv res ast)
  
          (= (Sym "def!") a0)
          (setv res (env-set env a1 (EVAL a2 env)))
  
          (= (Sym "let*") a0)
          (do
            (setv env (env-new env))
            (for [[b e] (partition a1 2)]
              (env-set env b (EVAL e env)))
            (setv ast a2)
            (continue)) ;; TCO

          (= (Sym "quote") a0)
          (setv res a1)

          (= (Sym "quasiquote") a0)
          (do (setv ast (QUASIQUOTE a1)) (continue)) ;; TCO
  
          (= (Sym "do") a0)
          (do (eval-ast (list (butlast (rest ast))) env)
              (setv ast (last ast))
              (continue)) ;; TCO
  
          (= (Sym "if") a0)
          (do
            (setv cond (EVAL a1 env))
            (if (or (none? cond) (and (instance? bool cond)
                                      (= cond False)))
              (if (> (len ast) 2)
                (do (setv ast (nth ast 3)) (continue)) ;; TCO
                (setv res None))
              (do (setv ast a2) (continue)))) ;; TCO
  
          (= (Sym "fn*") a0)
          (setv func (fn [&rest args]
                       (EVAL a2 (env-new env a1 (or args []))))
                func.ast a2
                func.env env
                func.params a1
                res func)

          ;; apply
          (do
            (setv el (eval-ast ast env)
                  f (first el)
                  args (list (rest el)))
            (if (hasattr f "ast")
              (do (setv ast f.ast
                        env (env-new f.env f.params args))
                  (continue)) ;; TCO
              (setv res (apply f args)))))))
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
(env-set repl-env (Sym "eval") (fn [ast] (EVAL ast repl-env)))
(env-set repl-env (Sym "*ARGV*") (tuple (map Str (rest (rest sys.argv)))))

;; core.mal: defined using the language itself
(REP "(def! not (fn* [a] (if a false true)))")
(REP "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")

(when (>= (len sys.argv) 2)
  (REP (+ "(load-file \"" (get sys.argv 1) "\")"))
  (sys.exit 0)) 

(while True
  (try
    (do (setv line (raw_input "user> "))
        (if (= "" line) (continue))
        (print (REP line)))
    (except [EOFError] (break))
    (except [Blank])
    (except []
      (print (.join "" (apply traceback.format_exception (.exc_info sys)))))))
