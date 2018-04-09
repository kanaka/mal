#! /usr/bin/env hy

(import [reader [read_str]])
(import [printer [pr_str]])

(defn READ [arg]
  (read_str arg))

(defn EVAL [arg]
  arg)

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
