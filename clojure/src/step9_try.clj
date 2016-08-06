(ns step9-try
    (:refer-clojure :exclude [macroexpand])
    (:require [clojure.repl]
              [readline]
              [reader]
              [printer]
              [env]
              [core])
    (:gen-class))

;; read
(defn READ [& [strng]]
  (let [line (if strng strng (read-line))]
    (reader/read-string strng)))

;; eval
(declare EVAL)
(defn is-pair [x]
  (and (sequential? x) (> (count x) 0)))

(defn quasiquote [ast]
  (cond
    (not (is-pair ast))
    (list 'quote ast)

    (= 'unquote (first ast))
    (second ast)

    (and (is-pair (first ast)) (= 'splice-unquote (ffirst ast)))
    (list 'concat (-> ast first second) (quasiquote (rest ast)))

    :else
    (list 'cons (quasiquote (first ast)) (quasiquote (rest ast)))))

(defn is-macro-call [ast env]
  (and (seq? ast)
       (symbol? (first ast))
       (env/env-find env (first ast))
       (:ismacro (meta (env/env-get env (first ast))))))

(defn macroexpand [ast env]
  (loop [ast ast]
    (if (is-macro-call ast env)
      (let [mac (env/env-get env (first ast))]
        (recur (apply mac (rest ast))))
      ast)))

(defn eval-ast [ast env]
  (cond
    (symbol? ast) (env/env-get env ast)

    (seq? ast)    (doall (map #(EVAL % env) ast))

    (vector? ast) (vec (doall (map #(EVAL % env) ast)))

    (map? ast)    (apply hash-map (doall (map #(EVAL % env)
                                              (mapcat identity ast))))

    :else         ast))

(defn EVAL [ast env]
  (loop [ast ast
         env env]
    ;;(prn "EVAL" ast (keys @env)) (flush)
    (if (not (seq? ast))
      (eval-ast ast env)

      ;; apply list
      (let [ast (macroexpand ast env)]
        (if (not (seq? ast))
          (eval-ast ast env)

          (let [[a0 a1 a2 a3] ast]
            (condp = a0
              nil
              ast

              'def!
              (env/env-set env a1 (EVAL a2 env))

              'let*
              (let [let-env (env/env env)]
                (doseq [[b e] (partition 2 a1)]
                  (env/env-set let-env b (EVAL e let-env)))
                (recur a2 let-env))

              'quote
              a1

              'quasiquote
              (recur (quasiquote a1) env)

              'defmacro!
              (let [func (with-meta (EVAL a2 env)
                                    {:ismacro true})]
                (env/env-set env a1 func))

              'macroexpand
              (macroexpand a1 env)

              'try*
              (if (= 'catch* (nth a2 0))
                (try
                  (EVAL a1 env)
                  (catch clojure.lang.ExceptionInfo ei
                    (EVAL (nth a2 2) (env/env env
                                              [(nth a2 1)]
                                              [(:data (ex-data ei))])))
                  (catch Throwable t
                    (EVAL (nth a2 2) (env/env env
                                              [(nth a2 1)]
                                              [(.getMessage t)]))))
                (EVAL a1 env))

              'do
              (do (eval-ast (->> ast (drop-last) (drop 1)) env)
                  (recur (last ast) env))

              'if
              (let [cond (EVAL a1 env)]
                (if (or (= cond nil) (= cond false))
                  (if (> (count ast) 2)
                    (recur a3 env)
                    nil)
                  (recur a2 env)))

              'fn*
              (with-meta
                (fn [& args]
                  (EVAL a2 (env/env env a1 (or args '()))))
                {:expression a2
                 :environment env
                 :parameters a1})

              ;; apply
              (let [el (eval-ast ast env)
                    f (first el)
                    args (rest el)
                    {:keys [expression environment parameters]} (meta f)]
                (if expression
                  (recur expression (env/env environment parameters args))
                  (apply f args))))))))))

;; print
(defn PRINT [exp] (pr-str exp))

;; repl
(def repl-env (env/env))
(defn rep
  [strng]
  (PRINT (EVAL (READ strng) repl-env)))

;; core.clj: defined using Clojure
(doseq [[k v] core/core_ns] (env/env-set repl-env k v))
(env/env-set repl-env 'eval (fn [ast] (EVAL ast repl-env)))
(env/env-set repl-env '*ARGV* ())

;; core.mal: defined using the language itself
(rep "(def! not (fn* [a] (if a false true)))")
(rep "(def! load-file (fn* [f] (eval (read-string (str \"(do \" (slurp f) \")\")))))")
(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
(rep "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")

;; repl loop
(defn repl-loop []
  (let [line (readline/readline "user> ")]
    (when line
      (when-not (re-seq #"^\s*$|^\s*;.*$" line) ; blank/comment
        (try
          (println (rep line))
          (catch Throwable e
            (clojure.repl/pst e))))
      (recur))))

(defn -main [& args]
  (env/env-set repl-env '*ARGV* (rest args))
  (if args
    (rep (str "(load-file \"" (first args) "\")"))
    (repl-loop)))
