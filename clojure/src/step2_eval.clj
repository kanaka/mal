(ns step2-eval
    (:require [clojure.repl]
              [readline]
              [reader]
              [printer]))

;; read
(defn READ [& [strng]]
  (let [line (if strng strng (read-line))]
    (reader/read-string strng)))

;; eval
(declare EVAL)
(defn eval-ast [ast env]
  (cond
    (symbol? ast) (or (get env ast)
                      (throw (Error. (str ast " not found"))))
   
    (seq? ast)    (doall (map #(EVAL % env) ast))

    (vector? ast) (vec (doall (map #(EVAL % env) ast)))
   
    (map? ast)    (apply hash-map (doall (map #(EVAL % env)
                                              (mapcat identity ast))))

    :else         ast))

(defn EVAL [ast env]
  ;;(prn "EVAL" ast (keys @env)) (flush)
  (if (not (seq? ast))
    (eval-ast ast env)

    ;; apply list
    (let [el (eval-ast ast env)
          f (first el)
          args (rest el)]
      (apply f args))))

;; print
(defn PRINT [exp] (pr-str exp))

;; repl
(def repl-env {'+ +
               '- -
               '* *
               '/ /})
(defn rep
  [strng]
  (PRINT (EVAL (READ strng) repl-env)))

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
