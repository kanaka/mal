(defn make-nil
  []
  {:tag :nil
   :content "nil"})

(defn make-boolean
  [bool]
  {:tag :boolean
   :content (string bool)})

(defn make-keyword
  [a-str]
  {:tag :keyword
   :content a-str})

(defn make-number
  [a-str]
  {:tag :number
   :content a-str})

(defn make-string
  [a-str]
  {:tag :string
   :content a-str})

(defn make-symbol
  [a-str]
  {:tag :symbol
   :content a-str})

(defn make-hash-map
  [items]
  (let [a-struct (if (dictionary? items)
                   items
                   (struct ;items))]
    {:tag :hash-map
     :content a-struct}))

(defn make-list
  [items]
  {:tag :list
   :content items})

(defn make-vector
  [items]
  {:tag :vector
   :content items})

(defn make-function
  [a-fn &opt ast params env]
  @{:tag :function
    :content a-fn
    :ast ast
    :params params
    :env env
    :is-macro false})

(defn make-atom
  [ast]
  @{:tag :atom
    :content ast})

(defn make-exception
  [ast]
  {:tag :exception
   :content ast})
