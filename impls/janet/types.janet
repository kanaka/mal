(defn make-number
  [a-str]
  {:tag :number
   :content a-str})

(defn make-symbol
  [a-str]
  {:tag :symbol
   :content a-str})

(defn make-hash-map
  [items]
  {:tag :hash-map
   :content items})

(defn make-list
  [items]
  {:tag :list
   :content items})

(defn make-vector
  [items]
  {:tag :vector
   :content items})
