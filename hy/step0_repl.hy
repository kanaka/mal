#! /usr/bin/env hy

(defn READ [arg]
  arg)

(defn EVAL [arg]
  arg)

(defn PRINT [arg]
  arg)

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
