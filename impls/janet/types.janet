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

(defn set-atom-value!
  [atom-ast value-ast]
  (put atom-ast
       :content value-ast))

(defn make-exception
  [ast]
  {:tag :exception
   :content ast})

## common accessors

(defn get-value
  [ast]
  (ast :content))

(defn get-type
  [ast]
  (ast :tag))

(defn get-meta
  [ast]
  (ast :meta))

## function-specific accessors

(defn get-is-macro
  [ast]
  (ast :is-macro))

(defn get-ast
  [ast]
  (ast :ast))

(defn get-params
  [ast]
  (ast :params))

(defn get-env
  [ast]
  (ast :env))

## function-specific functions

(defn macrofy
  [fn-ast]
  (merge fn-ast {:is-macro true}))

(defn clone-with-meta
  [fn-ast meta-ast]
  (merge fn-ast {:meta meta-ast}))

## predicates

(defn nil?*
  [ast]
  (= :nil (get-type ast)))

(defn boolean?*
  [ast]
  (= :boolean (get-type ast)))

(defn true?*
  [ast]
  (and (boolean?* ast)
       (= "true" (get-value ast))))

(defn false?*
  [ast]
  (and (boolean?* ast)
       (= "false" (get-value ast))))

(defn number?*
  [ast]
  (= :number (get-type ast)))

(defn symbol?*
  [ast]
  (= :symbol (get-type ast)))

(defn keyword?*
  [ast]
  (= :keyword (get-type ast)))

(defn string?*
  [ast]
  (= :string (get-type ast)))

(defn list?*
  [ast]
  (= :list (get-type ast)))

(defn vector?*
  [ast]
  (= :vector (get-type ast)))

(defn hash-map?*
  [ast]
  (= :hash-map (get-type ast)))

(defn fn?*
  [ast]
  (= :function (get-type ast)))

(defn macro?*
  [ast]
  (and (fn?* ast)
       (get-is-macro ast)))

(defn atom?*
  [ast]
  (= :atom (get-type ast)))

(defn exception?*
  [ast]
  (= :exception (get-type ast)))

(defn empty?*
  [ast]
  (empty? (get-value ast)))

# XXX: likely this could be simpler
(defn equals?*
  [ast-1 ast-2]
  (let [type-1 (get-type ast-1)
        type-2 (get-type ast-2)]
    (if (and (not= type-1 type-2)
             # XXX: not elegant
             (not (and (list?* ast-1) (vector?* ast-2)))
             (not (and (list?* ast-2) (vector?* ast-1))))
      false
      (let [val-1 (get-value ast-1)
            val-2 (get-value ast-2)]
        # XXX: when not a collection...
        (if (and (not (list?* ast-1))
                 (not (vector?* ast-1))
                 (not (hash-map?* ast-1)))
          (= val-1 val-2)
          (if (not= (length val-1) (length val-2))
            false
            (if (and (not (hash-map?* ast-1))
                     (not (hash-map?* ast-2)))
              (do
                (var found-unequal false)
                (each [v1 v2] (partition 2 (interleave val-1 val-2))
                      (when (not (equals?* v1 v2))
                        (set found-unequal true)
                        (break)))
                (not found-unequal))
              (if (or (not (hash-map?* ast-1))
                      (not (hash-map?* ast-2)))
                false
                (do
                  (var found-unequal false)
                  (each [k1 k2] (partition 2 (interleave (keys val-1)
                                                         (keys val-2)))
                        (when (not (equals?* k1 k2))
                          (set found-unequal true)
                          (break))
                        (when (not (equals?* (val-1 k1) (val-2 k2)))
                          (set found-unequal true)
                          (break)))
                  (not found-unequal))))))))))

## highlander types

(def mal-nil
  (make-nil))

(def mal-true
  (make-boolean true))

(def mal-false
  (make-boolean false))
