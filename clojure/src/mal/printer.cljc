(ns mal.printer
  (:refer-clojure :exclude [pr-str])
  (:require [clojure.string :as S]))

;; atom?
#?(:clj  (defn atom? [atm] (= (type atm) clojure.lang.Atom))
   :cljs (defn atom? [atm] (satisfies? IAtom atm)))

(defn escape [s]
  (-> s (S/replace "\\" "\\\\")
        (S/replace "\"" "\\\"")
        (S/replace "\n" "\\n")))

(defn pr-str
  ([obj] (pr-str obj true))
  ([obj print-readably]
   (let [_r print-readably]
     (cond
       (= nil obj)   "nil"
       (string? obj) (if _r (str "\"" (escape obj) "\"") obj)

       (list? obj)   (str "(" (S/join " " (map #(pr-str % _r) obj)) ")")
       (vector? obj) (str "[" (S/join " " (map #(pr-str % _r) obj)) "]")
       (map? obj)    (str "{" (S/join " " (map (fn [[k v]]
                                                 (str (pr-str k _r) " "
                                                      (pr-str v _r))) obj)) "}")
       (atom? obj)   (str "(atom " (pr-str @obj _r) ")")
       :else         (str obj)))))

