(import [hy.models [HyString :as Str]])
(import [mal_types [Atom]])
(import [reader [read-str]])
(import [printer [pr-str]])

(defn equal [a b]
  (if (and (coll? a) (coll? b) (= (len a) (len b)))
    (every? (fn [[a b]] (equal a b)) (zip a b))
    (= a b)))

(def ns
  {"=" equal

   "pr-str"  (fn [&rest a] (Str (.join " " (map (fn [e] (pr-str e True)) a))))
   "str"     (fn [&rest a] (Str (.join "" (map (fn [e] (pr-str e False)) a))))
   "prn"     (fn [&rest a] (print (.join " " (map (fn [e] (pr-str e True)) a))))
   "println" (fn [&rest a] (print (.join " " (map (fn [e] (pr-str e False)) a))))
   "read-string" read-str
   "slurp"   (fn [a] (Str (-> a open .read)))
  
   "<"  <
   "<=" <=
   ">"  >
   ">=" >=
   "+"  +
   "-"  -
   "*"  *
   "/"  (fn [a b] (int (/ a b)))

   "list"   (fn [&rest args] (tuple args))
   "list?"  (fn [a] (instance? tuple a))

   "empty?" empty?
   "count"  (fn [a] (if (none? a) 0 (len a)))

   "atom"   (fn [a] (Atom a))
   "atom?"  (fn [a] (instance? Atom a))
   "deref"  (fn [a] a.val)
   "reset!" (fn [a b] (do (setv a.val b) b))
   "swap!"  (fn [a f &rest args] (do (setv a.val (apply f (+ (, a.val) args)))
                                     a.val))
   })
