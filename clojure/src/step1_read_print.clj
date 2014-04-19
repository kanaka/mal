(ns step1-read-print
    (:require [clojure.repl]
              [readline]
              [reader]
              [printer]))

;; read
(defn READ [& [strng]]
  (let [line (if strng strng (read-line))]
    (reader/read-string strng)))

;; eval
(defn EVAL [ast env]
  ast)

;; print
(defn PRINT [exp] (pr-str exp))

;; repl
(defn rep
  [strng]
  (PRINT (EVAL (READ strng) {})))

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
