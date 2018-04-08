#! /usr/bin/env hy

(import types)
(import [hy.models [HySymbol HyList HySet]])

(defn pr_str [arg]
  (setv arg_type (type arg))
  (if (= HySymbol arg_type) (arg.format)
      (none? arg) "nil"
      (= bool arg_type) (if arg "true" "false")
      (= int arg_type) (str arg)
      (= HySet arg_type) (.format
                         "{{{0}}}"
                         (.join " " (list-comp (pr_str node) [node arg])))
      (= HyList arg_type) (.format
                          "[{0}]"
                          (.join " " (list-comp (pr_str node) [node arg])))
      (= list arg_type) (.format
                          "({0})"
                          (.join " " (list-comp (pr_str node) [node arg])))))

