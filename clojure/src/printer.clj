(ns printer)

(defmethod clojure.core/print-method clojure.lang.Atom [a writer]
  (.write writer "(atom ")
  (.write writer (pr-str @a))
  (.write writer ")"))

