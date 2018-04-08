#! /usr/bin/env hy

(import [reader [read_str]])
(import [printer [pr_str]])
(import [hy.models [HySymbol :as sym]])

(setv repl_env {"+" (fn [a b] (+ a b))
                "-" (fn [a b] (- a b))
                "*" (fn [a b] (* a b))
                "/" (fn [a b] (int (/ a b)))})

(defn eval_ast [ast]
  (if (= list (type ast)) (return (list-comp
                                    (eval_ast e)
                                    [e ast]))
      (= dict (type ast)) (return (dict-comp
                                    key (eval_ast value)
                                    [[key value] (.items ast)])))
  (if-not (and (= tuple (type ast))
               (= 3 (len ast))
               (= sym (type (get ast 0))))
          (return ast))
  ((get repl_env (get ast 0))
    (eval_ast (get ast 1))
    (eval_ast (get ast 2))))

(defn READ [arg]
  (read_str arg))

(defn EVAL [ast]
  (eval_ast ast))

(defn PRINT [arg]
  (pr_str arg))

(defn rep [arg]
  (PRINT (EVAL (READ arg))))

(defmain [&rest args]
  (while True
    (try
      (do
        (setv arg (input "user> "))
        (when (= "" arg) (continue))
        (print (rep arg)))
      (except [e EOFError] (break)))))
