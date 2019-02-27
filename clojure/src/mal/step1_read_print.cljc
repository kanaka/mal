(ns mal.step1-read-print
  (:require [mal.readline :as readline]
            #?(:clj [clojure.repl])
            [mal.reader :as reader]
            [mal.printer :as printer])
  #?(:clj (:gen-class)))

;; read
(defn READ [& [strng]]
  (reader/read-string strng))

;; eval
(defn EVAL [ast env]
  ast)

;; print
(defn PRINT [exp] (printer/pr-str exp))

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
          #?(:clj  (catch Throwable e (clojure.repl/pst e))
             :cljs (catch js/Error e (println (.-stack e))))))
      (recur))))

(defn -main [& args]
  (repl-loop))
