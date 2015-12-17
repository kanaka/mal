(ns printer)

(declare pr-seq)

(defn mal-pr-str [obj readable?]
  (cond
    (nil? obj)
    "nil"

    (vector? obj)
    (pr-seq obj "[" "]" readable?)

    (seq? obj)
    (pr-seq obj "(" ")" readable?)

    (map? obj)
    (pr-seq (mapcat identity obj) "{" "}" readable?)

    (= (type obj) clojure.lang.Atom)
    (str "(atom" (mal-pr-str @obj true) ")")

    :else
    ((if readable? pr-str str) obj)))

(defn pr-seq [elements start end readable?]
  (str start
       (clojure.string/join " " (map #(mal-pr-str % readable?) elements))
       end))
