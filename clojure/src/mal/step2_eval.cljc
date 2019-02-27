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
(declare EVAL)
(defn eval-ast [ast env]
  (cond
    (symbol? ast) (or (get env ast)
                      (throw (#?(:clj Error.
                                 :cljs js/Error.) (str ast " not found"))))

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
            (if (empty? ast)
              ast
              (let [el (eval-ast ast env)
                    f (first el)
                    args (rest el)]
                (apply f args)))))

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
