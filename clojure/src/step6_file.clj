(ns step6-file
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
      (let [[a0 a1 a2 a3] ast]
        (condp = a0
          'def!
          (types/env-set env a1 (EVAL a2 env))
  
          'let*
          (let [let-env (types/env env)]
            (doseq [[b e] (partition 2 a1)]
              (types/env-set let-env b (EVAL e let-env)))
            (EVAL a2 let-env))
  
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
              (apply f args))))))))

;; print
(defn PRINT [exp] (pr-str exp))

;; repl
(def repl-env (types/env))
(defn rep
  [strng]
  (PRINT (EVAL (READ strng), repl-env)))

(defn _ref [k,v] (types/env-set repl-env k v))

;; Import types related functions
(doseq [[k v] types/types_ns] (_ref k v))

;; Defined using the language itself
(_ref 'read-string reader/read-string)
(_ref 'eval (fn [ast] (EVAL ast repl-env)))
(_ref 'slurp slurp)

(rep "(def! not (fn* [a] (if a false true)))")
(rep "(def! load-file (fn* [f] (eval (read-string (str \"(do \" (slurp f) \")\")))))")

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
