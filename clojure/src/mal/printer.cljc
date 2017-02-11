(ns mal.printer)

#?(:clj (import '(java.io Writer)))

;; TODO Better:
;; (extend-protocol IPrintWithWriter
;;   Atom
;;   ...
;;   PersistentArrayMap
;;   ...
;;   PersistentHashMap
;;   ...)

;; Override atom printer
#?(:clj  (defmethod clojure.core/print-method clojure.lang.Atom [a writer]
           (.write writer "(atom ")
           (.write writer (pr-str @a))
           (.write writer ")"))
   :cljs (extend-type Atom
           IPrintWithWriter
           (-pr-writer [a writer _]
             (-write writer (str "(atom " (pr-str @a) ")")))))


;; Override hash-map printer to remove comma separators
#?(:clj  (defmethod print-method clojure.lang.IPersistentMap [hm ^Writer w]
           (.write w "{")
           (when-let [xs (seq hm)]
             (loop [[[k v] & xs] xs]
               (print-method k w)
               (.write w " ")
               (print-method v w)
               (when xs (.write w " ") (recur xs))))
           (.write w "}"))
   :cljs (extend-type PersistentHashMap
           IPrintWithWriter
           (-pr-writer [hm w _]
             (-write w "{")
             (when-let [xs (seq hm)]
               (loop [[[k v] & xs] xs]
                 (-write w (pr-str k))
                 (-write w " ")
                 (-write w (pr-str v))
                 (when xs (-write w " ") (recur xs))))
             (-write w "}"))))


;; Add a version of str that is the same all the way down (no
;; print-readably and nil printing all the way down)
(defn- pr-
  ([] nil)
  ([x]
     #?(:clj (print-method x *out*)
        :cljs (pr x)))
  ([x & more]
   (pr- x)
   (if-let [nmore (next more)]
     (recur (first more) nmore)
     (apply pr- more))))

(defn _str [& xs]
  (binding [*print-readably* nil]
    (with-out-str (apply pr- xs))))
