(ns mal.step4-if-fn-do
  (:require [mal.readline :as readline]
            #?(:clj [clojure.repl])
            [mal.reader :as reader]
            [mal.printer :as printer]
            [mal.env :as env]
            [mal.core :as core])
  #?(:clj (:gen-class)))

;; read
(defn READ [& [strng]]
  (reader/read-string strng))

;; eval
(defn EVAL [ast env]

  (let [e (env/env-find env 'DEBUG-EVAL)]
    (when e
      (let [v (env/env-get e 'DEBUG-EVAL)]
        (when (and (not= v nil)
                   (not= v false))
          (println "EVAL:" (printer/pr-str ast) (keys @env))
          (flush)))))

  (cond
    (symbol? ast) (env/env-get env ast)

    (vector? ast) (vec (map #(EVAL % env) ast))

    (map? ast) (apply hash-map (map #(EVAL % env) (mapcat identity ast)))

    (seq? ast)
      ;; apply list
          ;; indented to match later steps
          (let [[a0 a1 a2 a3] ast]
            (condp = a0
              nil
              ast

              'def!
              (env/env-set env a1 (EVAL a2 env))

              'let*
              (let [let-env (env/env env)]
                (doseq [[b e] (partition 2 a1)]
                  (env/env-set let-env b (EVAL e let-env)))
                (EVAL a2 let-env))

              'do
              (last (doall (map #(EVAL % env) (rest ast))))

              'if
              (let [cond (EVAL a1 env)]
                (if (or (= cond nil) (= cond false))
                  (if (> (count ast) 2)
                    (EVAL a3 env)
                    nil)
                  (EVAL a2 env)))

              'fn*
              (fn [& args]
                (EVAL a2 (env/env env a1 (or args '()))))

              ;; apply
              (let [el (map #(EVAL % env) ast)
                    f (first el)
                    args (rest el)]
                (apply f args))))

    :else ;; not a list, map, symbol or vector
    ast))

;; print
(defn PRINT [exp] (printer/pr-str exp))

;; repl
(def repl-env (env/env))
(defn rep
  [strng]
  (PRINT (EVAL (READ strng) repl-env)))

;; core.clj: defined using Clojure
(doseq [[k v] core/core_ns] (env/env-set repl-env k v))

;; core.mal: defined using the language itself
(rep "(def! not (fn* [a] (if a false true)))")

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
