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
  [a-num]
  {:tag :number
   :content a-num})

(defn make-string
  [a-str]
  {:tag :string
   :content a-str})

(defn make-symbol
  [a-str]
  {:tag :symbol
   :content a-str})

(defn make-hash-map
  [items &opt meta]
  (default meta (make-nil))
  (let [a-struct (if (dictionary? items)
                   items
                   (struct ;items))]
    {:tag :hash-map
     :content a-struct
     :meta meta}))

(defn make-list
  [items &opt meta]
  (default meta (make-nil))
  {:tag :list
   :content items
   :meta meta})

(defn make-vector
  [items &opt meta]
  (default meta (make-nil))
  {:tag :vector
   :content items
   :meta meta})

(defn make-function
  [a-fn &opt meta is-macro ast params env]
  (default meta (make-nil))
  (default is-macro false)
  {:tag :function
   :content a-fn
   :meta meta
   :is-macro is-macro
   :ast ast
   :params params
   :env env})

(defn make-atom
  [ast]
  @{:tag :atom
    :content ast})

(defn make-exception
  [ast]
  {:tag :exception
   :content ast})
