(ns mal.step0-repl
  (:require [mal.readline :as readline])
  #?(:clj (:gen-class)))

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
      (when-not (re-seq #"^\s*$|^\s*;.*$" line) ; blank/comment
        (println (rep line)))
      (recur))))

(defn -main [& args]
  (repl-loop))
