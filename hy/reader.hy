#! /usr/bin/env hy

(import re)
(import [hy.models [HySymbol :as sym]])

(defclass Reader []
  (defn __init__ [self tokens &optional [position 0]]
    (setv self.tokens tokens)
    (setv self.position position))
  (defn next [self]
    (setv self.position (+ self.position 1))
    (get self.tokens (- self.position 1)))
  (defn peek [self]
    (if (> (len self.tokens) self.position)
        (get self.tokens self.position)
        None)))

(defn tokenizer [str]
  (re.findall
    r"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\"])*\"?|'[^\s]*|;.*|[^\s\[\]{}('\"`,;)]*)"
    str))

(defn read_str [str]
  (setv tokens (tokenizer str))
  (setv reader (Reader tokens))
  (try
    (read_form reader)
    (except [e ValueError]
      (print e))))

(defn read_form [rdr]
  (setv token (.peek rdr))
  (if (= "(" token) (do (.next rdr) (read_s_exp rdr))
      (= "[" token) (do (.next rdr) (read_list rdr))
      (= "{" token) (do (.next rdr) (read_hash_map rdr))
      (= "'" token) (do (.next rdr) (read_quote rdr))
      (= "`" token) (do (.next rdr) (read_quasiquote rdr))
      (= "~" token) (do (.next rdr) (read_unquote rdr))
      (= "~@" token) (do (.next rdr) (read_splice_unquote rdr))
      (= "@" token) (do (.next rdr) (read_deref rdr))
      (= "^" token) (do (.next rdr) (read_with_meta rdr))
      (and (= "\"" (get token 0))
           (!= "\"" (get token -1))) (raise (ValueError "expected '\"', got EOF"))
      True (read_atom rdr)))

(defn read_with_meta [rdr]
  (setv arg2 (read-form rdr))
  (setv arg1 (read-form rdr))
  (tuple [(sym "with-meta") arg1 arg2]))

(defn read_deref [rdr]
  (tuple [(sym "deref") (read-form rdr)]))

(defn read_hash_map [rdr]
  (setv end "}")
  (setv result {})
  (while True
    (setv key (.peek rdr))
    (if
      (= end key) (do (.next rdr) (return result))
      (= "" key) (raise (ValueError (.format "expected '{0}', get EOF" end))))
    (setv key (read_form rdr))
    (setv token (read_form rdr))
    (.update result {key token})))

(defn read_quote [rdr]
  (tuple [(sym "quote") (read-form rdr)]))

(defn read_quasiquote [rdr]
  (tuple [(sym "quasiquote") (read-form rdr)]))

(defn read_unquote [rdr]
  (tuple [(sym "unquote") (read-form rdr)]))

(defn read_splice_unquote [rdr]
  (tuple [(sym "splice-unquote") (read-form rdr)]))

(defn read_list [rdr]
  (setv end "]")
  (setv result [])
  (while True
    (setv token (.peek rdr))
    (if
      (= end token) (do (.next rdr) (return result))
      (= "" token) (raise (ValueError (.format "expected '{0}', got EOF" end))))
    (.append result (read_form rdr))))

(defn read_s_exp [rdr]
  (setv end ")")
  (setv result [])
  (while True
    (setv token (.peek rdr))
    (if
      (= end token) (do (.next rdr) (return (tuple result)))
      (= "" token) (raise (ValueError (.format "expected '{0}', got EOF" end))))
    (.append result (read_form rdr))))

(defn read_atom [rdr]
  (setv token (.next rdr))
  (if
    (re.match r"-?[0-9]+$" token) (int token)
    (= "nil" token) None
    (= "true" token) True
    (= "false" token) False
    True (sym token)))
