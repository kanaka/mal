(import [hy.models [HyInteger :as Int HyKeyword :as Keyword
                    HyString :as Str HySymbol :as Sym HyDict :as Map]])
(import [mal_types [Atom]])

(defn escape [s]
  (-> (str s) (.replace "\\" "\\\\")
              (.replace "\"" "\\\"")
              (.replace "\n" "\\n")))

(defn pr-str [obj &optional [print-readably True]]
  (setv _r print-readably
        t (type obj))
  (Str
    (if
      (none? obj)   "nil"
      (= t bool)    (if obj "true" "false")
      (= t Keyword) (+ ":" (name obj))
      (= t Str)     (if _r (+ "\"" (escape obj) "\"") obj)
      (= t tuple)   (+ "(" (.join " " (map (fn [x] (pr-str x _r)) obj)) ")")
      (= t list)    (+ "[" (.join " " (map (fn [x] (pr-str x _r)) obj)) "]")
      (= t Map)     (+ "{" (.join " " (map (fn [x] (pr-str x _r)) obj)) "}")
      (instance? Atom obj) (+ "(atom " (pr-str obj.val _r) ")")
      True          (str obj))))
