(ns mal.step2-eval
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

  ;; (println "EVAL:" (printer/pr-str ast) (keys @env))
  ;; (flush)

  (cond
    (symbol? ast) (or (get env ast)
                      (throw (#?(:clj Error.
                                 :cljs js/Error.) (str ast " not found"))))

    (vector? ast) (vec (map #(EVAL % env) ast))

    (map? ast) (apply hash-map (map #(EVAL % env) (mapcat identity ast)))

    (seq? ast)
      ;; apply list
            ;; indented to match later steps
            (if (empty? ast)
              ast
              (let [el (map #(EVAL % env) ast)
                    f (first el)
                    args (rest el)]
                (apply f args)))

    :else ;; not a list, map, symbol or vector
    ast))

;; print
(defn PRINT [exp] (printer/pr-str exp))

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
          #?(:clj  (catch Throwable e (clojure.repl/pst e))
             :cljs (catch js/Error e (println (.-stack e))))))
      (recur))))

(defn -main [& args]
  (repl-loop))
