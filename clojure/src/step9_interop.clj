(ns step9-interop
    (:refer-clojure :exclude [macroexpand])
    (:require [clojure.repl]
              [types]
              [readline]
              [reader]))

(declare EVAL)

;; read
(defn READ [& [strng]]
  (let [line (if strng strng (read-line))]
    (reader/read-string strng)))

;; eval
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
       (types/env-find env (first ast))
       (:ismacro (meta (types/env-get env (first ast))))))

(defn macroexpand [ast env]
  (loop [ast ast]
    (if (is-macro-call ast env)
      (let [mac (types/env-get env (first ast))]
        (recur (apply mac (rest ast))))
      ast)))

(defn eval-ast [ast env]
  (cond
    (symbol? ast) (types/env-get env ast)
   
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
          ast

          (let [[a0 a1 a2 a3] ast]
            (condp = a0
              'def!
              (types/env-set env a1 (EVAL a2 env))
      
              'let*
              (let [let-env (types/env env)]
                (doseq [[b e] (partition 2 a1)]
                  (types/env-set let-env b (EVAL e let-env)))
                (EVAL a2 let-env))
    
              'quote
              a1
    
              'quasiquote
              (EVAL (quasiquote a1) env)
    
              'defmacro!
              (let [func (with-meta (EVAL a2 env)
                                    {:ismacro true})]
                (types/env-set env a1 func))
    
              'macroexpand
              (macroexpand a1 env)
      
              'clj*
              (eval (reader/read-string a1))
      
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
              ^{:expression a2
                :environment env
                :parameters a1}
              (fn [& args]
                (EVAL a2 (types/env env a1 args)))
      
              ;; apply
              (let [el (eval-ast ast env)
                       f (first el)
                       args (rest el)
                       {:keys [expression environment parameters]} (meta f)]
                (if expression
                  (recur expression (types/env environment parameters args))
                  (apply f args))))))))))

;; print
(defn PRINT [exp] (pr-str exp))

;; repl
(def repl-env (types/env))
(defn rep
  [strng]
  (PRINT (EVAL (READ strng) repl-env)))

(defn _ref [k,v] (types/env-set repl-env k v))

;; Import types related functions
(doseq [[k v] types/types_ns] (_ref k v))

;; Defined using the language itself
(_ref 'read-string reader/read-string)
(_ref 'eval (fn [ast] (EVAL ast repl-env)))
(_ref 'slurp slurp)
(_ref 'slurp-do (fn [f] (str "(do " (slurp f) ")")))

(rep "(def! not (fn* [a] (if a false true)))")
(rep "(def! load-file (fn* [f] (eval (read-string (slurp-do f)))))")

(defn -main [& args]
  (if args
    (rep (str "(load-file \"" (first args) "\")"))
    (loop []
      (let [line (readline/readline "user> ")]
        (when line
          (when-not (re-seq #"^\s*$|^\s*;.*$" line) ; blank/comment
            (try
              (println (rep line))
              (catch Throwable e
                (clojure.repl/pst e))))
          (recur))))))
