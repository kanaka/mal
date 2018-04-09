#! /usr/bin/env hy

(defclass Env []
  (defn __init__ [self]
    (setv self.data {:outer None}))
  (defn set [self key value]
    (assoc self.data key value))
  (defn find [self key]
    (try (get self.data key)
         (except [e KeyError]
           (setv outer (get self.data :outer))
           (if (none? outer)
               None
               (.find outer)))))
  (defn get [self key]
    (setv value (self.find key))
    (if (none? value)
        (raise (ValueError "not found"))
        value)))

(defmain [&rest args]
  (setv e (Env))
  (.set e "hoge" "hige")
  (.find e "hoge")
  (.find e "foo")
  (.get e "hoge")
  (.get e "foo")
  )
