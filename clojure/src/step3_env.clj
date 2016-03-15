(ns step3-env
    (:require [clojure.repl]
              [readline]
              [reader]
              [printer]
              [env]))

;; read
(defn READ [& [strng]]
  (let [line (if strng strng (read-line))]
    (reader/read-string strng)))

;; eval
(declare EVAL)
(defn eval-ast [ast env]
  (cond
    (symbol? ast) (env/env-get env ast)

    (seq? ast)    (doall (map #(EVAL % env) ast))

    (vector? ast) (vec (doall (map #(EVAL % env) ast)))

    (map? ast)    (apply hash-map (doall (map #(EVAL % env)
                                              (mapcat identity ast))))

    :else         ast))

(defn EVAL [ast env]
    ;; indented to match later steps
    ;;(prn "EVAL" ast (keys @env)) (flush)
    (if (not (seq? ast))
      (eval-ast ast env)

      ;; apply list
          ;; indented to match later steps
          (let [[a0 a1 a2 a3] ast]
            (condp = a0
              'def!
              (env/env-set env a1 (EVAL a2 env))

              'let*
              (let [let-env (env/env env)]
                (doseq [[b e] (partition 2 a1)]
                  (env/env-set let-env b (EVAL e let-env)))
                (EVAL a2 let-env))

              ;; apply
              (let [el (eval-ast ast env)
                    f (first el)
                    args (rest el)]
                (apply f args))))))

;; print
(defn PRINT [exp] (pr-str exp))

;; repl
(def repl-env (env/env))
(defn rep
  [strng]
  (PRINT (EVAL (READ strng) repl-env)))

(env/env-set repl-env '+ +)
(env/env-set repl-env '- -)
(env/env-set repl-env '* *)
(env/env-set repl-env '/ /)

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
  (repl-loop))
