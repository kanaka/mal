(ns step0-repl
  (:require [readline]))


;; read
(defn READ [& [strng]]
  strng)

;; eval
(defn EVAL [ast env]
  ast)

;; print
(defn PRINT [exp]
  exp)

;; repl
(defn rep [strng] (PRINT (EVAL (READ strng), {})))
;; repl loop
(defn repl-loop []
  (let [line (readline/readline "user> ")]
    (when line
      (println (rep line))
      (recur))))

(defn -main [& args]
  (repl-loop))
