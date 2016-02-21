(ns printer)

(import '(java.io Writer))

;; Override atom printer
(defmethod clojure.core/print-method clojure.lang.Atom [a writer]
  (.write writer "(atom ")
  (.write writer (pr-str @a))
  (.write writer ")"))


;; Override hash-map printer to remove comma separators
(defmethod print-method clojure.lang.IPersistentMap [hm, ^Writer w]
  (.write w "{")
  (when-let [xs (seq hm)]
    (loop [[[k v] & xs] xs]
      (print-method k w) (.append w \space) (print-method v w)
      (when xs (.write w " ") (recur xs))))
  (.write w "}"))


;; Add a version of str that is the same all the way down (no
;; print-readably and nil printing all the way down)
(defn- pr-
  ([] nil)
  ([x]
     (print-method x *out*))
  ([x & more]
   (pr- x)
   (if-let [nmore (next more)]
     (recur (first more) nmore)
     (apply pr- more))))

(defn _str [& xs]
  (binding [*print-readably* nil]
    (with-out-str (apply pr- xs))))
