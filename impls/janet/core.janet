(import ./types :prefix "")
(import ./printer)
(import ./reader)

(defn throw*
  [ast]
  (error ast))

(defn arith-fn
  [op]
  (make-function
    (fn [asts]
      (make-number
        (op ;(map |($ :content)
                  asts))))))

(def mal-list
  (make-function
    (fn [asts]
      (make-list asts))))

(defn list?*
  [ast]
  (= (ast :tag) :list))

(def mal-list?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "list? requires 1 argument")))
      (if (list?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn vector?*
  [ast]
  (= (ast :tag) :vector))

(def mal-vector?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "vector? requires 1 argument")))
      (if (vector?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn empty?*
  [ast]
  (empty? (ast :content)))

(def mal-empty?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "empty? requires 1 argument")))
      (if (empty?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn nil?*
  [ast]
  (= :nil (ast :tag)))

(def mal-nil?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "nil? requires 1 argument")))
      (if (nil?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(def mal-count
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "count requires 1 argument")))
      (let [ast (in asts 0)]
        (if (nil?* ast)
          (make-number 0)
          (make-number (length (ast :content))))))))

(defn cmp-fn
  [op]
  (make-function
    (fn [asts]
      (if (op ;(map |($ :content) asts))
        (make-boolean true)
        (make-boolean false)))))

(def mal-pr-str
  (make-function
    (fn [asts]
      (def buf @"")
      (when (> (length asts) 0)
        (each ast asts
              (buffer/push-string buf (printer/pr_str ast true))
              (buffer/push-string buf " "))
        # remove extra space at end
        (buffer/popn buf 1))
      (make-string (string buf)))))

(def mal-str
  (make-function
    (fn [asts]
      (def buf @"")
      (when (> (length asts) 0)
        (each ast asts
              (buffer/push-string buf (printer/pr_str ast false))))
      (make-string (string buf)))))

(def mal-prn
  (make-function
    (fn [asts]
      (def buf @"")
      (when (> (length asts) 0)
        (each ast asts
              (buffer/push-string buf (printer/pr_str ast true))
              (buffer/push-string buf " "))
        # remove extra space at end
        (buffer/popn buf 1))
      (print (string buf))
      (make-nil))))

(def mal-println
  (make-function
    (fn [asts]
      (def buf @"")
      (when (> (length asts) 0)
        (each ast asts
              (buffer/push-string buf (printer/pr_str ast false))
              (buffer/push-string buf " "))
        # remove extra space at end
        (buffer/popn buf 1))
      (print (string buf))
      (make-nil))))

(def mal-read-string
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "read-string requires 1 argument")))
      (if-let [res (reader/read_str ((in asts 0) :content))]
        res
        (throw* (make-string "No code content"))))))

(def mal-slurp
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "slurp requires 1 argument")))
      (let [a-str ((in asts 0) :content)]
        (if (not (os/stat a-str))
          (throw* (string "File not found: " a-str))
          # XXX: escaping?
          (make-string (slurp a-str)))))))

(def mal-atom
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "atom requires 1 argument")))
      (make-atom (in asts 0)))))

(defn atom?*
  [ast]
  (= (ast :tag) :atom))

(def mal-atom?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "atom? requires 1 argument")))
      (if (atom?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn deref*
  [ast]
  (if (not (atom?* ast))
    (throw* (make-string (string "Expected atom, got: " (ast :tag))))
    (ast :content)))

(def mal-deref
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "deref requires 1 argument")))
      (let [ast (in asts 0)]
        (deref* ast)))))

(defn reset!*
  [atom-ast val-ast]
  (put atom-ast
       :content val-ast)
  val-ast)

(def mal-reset!
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "reset! requires 2 arguments")))
      (let [atom-ast (in asts 0)
            val-ast (in asts 1)]
        (reset!* atom-ast val-ast)))))

(def mal-swap!
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "swap! requires at least 2 arguments")))
      (let [atom-ast (in asts 0)
            fn-ast (in asts 1)
            args-asts (slice asts 2)
            inner-ast (deref* atom-ast)]
        (reset!* atom-ast
                 ((fn-ast :content) [inner-ast ;args-asts]))))))

(defn cons*
  [head-ast tail-ast]
  [head-ast ;(tail-ast :content)])

(def mal-cons
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "cons requires 2 arguments")))
      (let [head-ast (in asts 0)
            tail-ast (in asts 1)]
        (make-list (cons* head-ast tail-ast))))))

(defn concat*
  [& list-asts]
  (reduce (fn [acc list-ast]
            [;acc ;(list-ast :content)])
          []
          list-asts))

(def mal-concat
  (make-function
    (fn [asts]
      (make-list (concat* ;asts)))))

(defn nth*
  [coll-ast num-ast]
  (let [elts (coll-ast :content)
        n-elts (length elts)
        i (num-ast :content)]
    (if (< i n-elts)
      (in elts i)
      (throw* (make-string (string "Index out of range: " i))))))

(def mal-nth
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "nth requires 2 arguments")))
      (let [coll-ast (in asts 0)
            num-ast (in asts 1)]
        (nth* coll-ast num-ast)))))

(defn first*
  [coll-or-nil-ast]
  (if (or (nil?* coll-or-nil-ast)
          (empty?* coll-or-nil-ast))
    (make-nil)
    (in (coll-or-nil-ast :content) 0)))

(def mal-first
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "first requires 1 argument")))
      (let [coll-or-nil-ast (in asts 0)]
        (first* coll-or-nil-ast)))))

(defn rest*
  [coll-or-nil-ast]
  (if (or (nil?* coll-or-nil-ast)
          (empty?* coll-or-nil-ast))
    (make-list [])
    (make-list (slice (coll-or-nil-ast :content) 1))))

(def mal-rest
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "rest requires 1 argument")))
      (let [coll-or-nil-ast (in asts 0)]
        (rest* coll-or-nil-ast)))))

(def mal-throw
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "throw requires 1 argument")))
      (throw* (in asts 0)))))

# (apply F A B [C D]) is equivalent to (F A B C D)
(def mal-apply
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "apply requires at least 1 argument")))
      (let [the-fn ((in asts 0) :content)] # e.g. F
        (if (= (length asts) 1)
          (the-fn [])
          (let [last-asts ((get (slice asts -2) 0) :content) # e.g. [C D]
                args-asts (slice asts 1 -2)] # e.g. [A B]
            (the-fn [;args-asts ;last-asts])))))))

(def mal-map
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "map requires at least 2 arguments")))
      (let [the-fn ((in asts 0) :content)
            coll ((in asts 1) :content)]
        (make-list (map |(the-fn [$])
                        coll))))))

(def mal-vec
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "vec requires 1 argument")))
      (let [ast (in asts 0)]
        (cond
          (vector?* ast)
          ast
          ##
          (list?* ast)
          (make-vector (ast :content))
          ##
          (nil?* ast)
          (make-vector ())
          ##
          (throw* (make-string "vec requires a vector, list, or nil")))))))

(defn true?*
  [ast]
  (and (= :boolean (ast :tag))
       (= "true" (ast :content))))

(def mal-true?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "true? requires 1 argument")))
      (if (true?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn false?*
  [ast]
  (and (= :boolean (ast :tag))
       (= "false" (ast :content))))

(def mal-false?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "false? requires 1 argument")))
      (if (false?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn symbol?*
  [ast]
  (= :symbol (ast :tag)))

(def mal-symbol?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "symbol? requires 1 argument")))
      (if (symbol?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(def mal-symbol
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "symbol requires 1 argument")))
      (make-symbol ((in asts 0) :content)))))

(defn keyword?*
  [ast]
  (= (ast :tag) :keyword))

(defn string?*
  [ast]
  (= (ast :tag) :string))

(def mal-keyword
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "keyword requires 1 argument")))
      (let [arg-ast (in asts 0)]
        (cond
          (keyword?* arg-ast)
          arg-ast
          ##
          (string?* arg-ast)
          (make-keyword (string ":" (arg-ast :content)))
          ##
          (throw* (make-string "Expected string")))))))

(def mal-keyword?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "keyword? requires 1 argument")))
      (if (keyword?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(def mal-string?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "string? requires 1 argument")))
      (if (string?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(def mal-vector
  (make-function
    (fn [asts]
      (make-vector asts))))

(def mal-sequential?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "sequential? requires 1 argument")))
      (if (or (list?* (in asts 0))
              (vector?* (in asts 0)))
        (make-boolean true)
        (make-boolean false)))))

(def mal-hash-map
  (make-function
    (fn [asts]
      (when (= 1 (% (length asts) 2))
        (throw* (make-string
                  "hash-map requires an even number of arguments")))
      (make-hash-map asts))))

(defn hash-map?*
  [ast]
  (= (ast :tag) :hash-map))

(def mal-map?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "map? requires 1 argument")))
      (if (hash-map?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(def mal-assoc
  (make-function
    (fn [asts]
      (when (< (length asts) 3)
        (throw* (make-string "assoc requires at least 3 arguments")))
      (let [head-ast (in asts 0)]
        (when (not (or (hash-map?* head-ast)
                       (nil?* head-ast)))
          (throw* (make-string
                    "assoc first argument should be a hash-map or nil")))
        (if (nil?* head-ast)
          (make-nil)
          (let [item-table (table ;(kvs (head-ast :content)))
                kv-asts (slice asts 1 -1)]
            (each [key-ast val-ast] (partition 2 kv-asts)
                  (put item-table key-ast val-ast))
            (make-hash-map (table/to-struct item-table))))))))

(def mal-dissoc
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "dissoc requires at least 2 arguments")))
      (let [head-ast (in asts 0)]
        (when (not (or (hash-map?* head-ast)
                       (nil?* head-ast)))
          (throw* (make-string
                    "dissoc first argument should be a hash-map or nil")))
        (if (nil?* head-ast)
          (make-nil)
          (let [item-table (table ;(kvs (head-ast :content)))
                key-asts (slice asts 1 -1)]
            (each key-ast key-asts
                  (put item-table key-ast nil))
            (make-hash-map (table/to-struct item-table))))))))

(def mal-get
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "get requires 2 arguments")))
      (let [head-ast (in asts 0)]
        (when (not (or (hash-map?* head-ast)
                       (nil?* head-ast)))
          (throw* (make-string
                    "get first argument should be a hash-map or nil")))
        (if (nil?* head-ast)
          (make-nil)
          (let [item-struct (head-ast :content)
                key-ast (in asts 1)]
            (if-let [val-ast (get item-struct key-ast)]
              val-ast
              (make-nil))))))))

(def mal-contains?
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "contains? requires 2 arguments")))
      (let [head-ast (in asts 0)]
        (when (not (or (hash-map?* head-ast)
                       (nil?* head-ast)))
          (throw* (make-string
                    "contains? first argument should be a hash-map or nil")))
        (if (nil?* head-ast)
          (make-nil)
          (let [item-struct (head-ast :content)
                key-ast (in asts 1)]
            (if-let [val-ast (get item-struct key-ast)]
              (make-boolean true)
              (make-boolean false))))))))

(def mal-keys
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "keys requires 1 argument")))
      (let [head-ast (in asts 0)]
        (when (not (or (hash-map?* head-ast)
                       (nil?* head-ast)))
          (throw* (make-string
                    "keys first argument should be a hash-map or nil")))
        (if (nil?* head-ast)
          (make-nil)
          (let [item-struct (head-ast :content)]
            (make-list (keys item-struct))))))))

(def mal-vals
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "vals requires 1 argument")))
      (let [head-ast (in asts 0)]
        (when (not (or (hash-map?* head-ast)
                       (nil?* head-ast)))
          (throw* (make-string
                    "vals first argument should be a hash-map or nil")))
        (if (nil?* head-ast)
          (make-nil)
          (let [item-struct (head-ast :content)]
            (make-list (values item-struct))))))))

# XXX: likely this could be simpler
(defn equals?*
  [ast-1 ast-2]
  (let [tag-1 (ast-1 :tag)
        tag-2 (ast-2 :tag)]
    (if (and (not= tag-1 tag-2)
             # XXX: not elegant
             (not (and (= tag-1 :list) (= tag-2 :vector)))
             (not (and (= tag-2 :list) (= tag-1 :vector))))
      false
      (let [val-1 (ast-1 :content)
            val-2 (ast-2 :content)]
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

(def mal-=
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "= requires 2 arguments")))
      (let [ast-1 (in asts 0)
            ast-2 (in asts 1)]
        (if (equals?* ast-1 ast-2)
          (make-boolean true)
          (make-boolean false))))))

(def mal-readline
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "readline requires 1 argument")))
      (let [prompt ((in asts 0) :content)
            buf @""]
        (file/write stdout prompt)
        (file/flush stdout)
        (file/read stdin :line buf)
        (if (< 0 (length buf))
          (make-string (string/trimr buf))
          (make-nil))))))

(defn number?*
  [ast]
  (= (ast :tag) :number))

(def mal-number?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "number? requires 1 argument")))
      (if (number?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn fn?*
  [ast]
  (= (ast :tag) :function))

(def mal-fn?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "fn? requires 1 argument")))
      (let [target-ast (in asts 0)]
        (if (and (fn?* target-ast)
                 (not (target-ast :is-macro)))
          (make-boolean true)
          (make-boolean false))))))

(defn macro?*
  [ast]
  (and (fn?* ast)
       (ast :is-macro)))

(def mal-macro?
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "macro? requires 1 argument")))
      (let [the-ast (in asts 0)]
        (if (macro?* the-ast)
          (make-boolean true)
          (make-boolean false))))))

(def mal-time-ms
  (make-function
    (fn [asts]
      (make-number (os/clock)))))

(def mal-conj
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "conj requires at least 2 arguments")))
      (let [coll-ast (in asts 0)
            item-asts (slice asts 1)]
        (cond
          (nil?* coll-ast)
          (make-list [;(reverse item-asts)])
          ##
          (list?* coll-ast)
          (make-list [;(reverse item-asts) ;(coll-ast :content)])
          ##
          (vector?* coll-ast)
          (make-vector [;(coll-ast :content) ;item-asts])
          ##
          (throw* (make-string "Expected list or vector")))))))

(def mal-seq
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "seq requires 1 argument")))
      (let [arg-ast (in asts 0)]
        (cond
          (list?* arg-ast)
          (if (empty?* arg-ast)
            (make-nil)
            arg-ast)
          ##
          (vector?* arg-ast)
          (if (empty?* arg-ast)
            (make-nil)
            (make-list (arg-ast :content)))
          ##
          (string?* arg-ast)
          (if (empty?* arg-ast)
            (make-nil)
            (let [str-asts (map |(make-string (string/from-bytes $))
                                (arg-ast :content))]
              (make-list str-asts)))
          ##
          (nil?* arg-ast)
          arg-ast
          ##
          (throw* (make-string "Expected list, vector, string, or nil")))))))

(def mal-meta
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "meta requires 1 argument")))
      (let [head-ast (in asts 0)]
        (if (or (list?* head-ast)
                (vector?* head-ast)
                (hash-map?* head-ast)
                (fn?* head-ast))
          ((in asts 0) :meta)
          (make-nil))))))

(defn spawn-function
  [fn-ast overrides]
  (merge fn-ast overrides))

(def mal-with-meta
  (make-function
    (fn [asts]
      (when (< (length asts) 2)
        (throw* (make-string "with-meta requires 2 arguments")))
      (let [target-ast (in asts 0)
            meta-ast (in asts 1)]
        (cond
          (list?* target-ast)
          (make-list (target-ast :content) meta-ast)
          ##
          (vector?* target-ast)
          (make-vector (target-ast :content) meta-ast)
          ##
          (hash-map?* target-ast)
          (make-hash-map (target-ast :content) meta-ast)
          ##
          (fn?* target-ast)
          (spawn-function target-ast {:meta meta-ast})
          ##
          (throw* (make-string "Expected list, vector, hash-map, or fn")))))))

(defn janet-eval*
  [janet-val]
  (case (type janet-val)
    :nil
    (make-nil)
    ##
    :boolean
    (make-boolean janet-val)
    ##
    :number # XXX: there may be some incompatibilities
    (make-number janet-val)
    ##
    :string
    (make-string janet-val)
    ##
    :keyword # XXX: there may be some incompatibilities
    (make-keyword (string ":" janet-val))
    ##
    :symbol # XXX: there may be some incompatibilities
    (make-symbol (string janet-val))
    ##
    :tuple
    (make-list (map janet-eval* janet-val))
    ##
    :array
    (make-list (map janet-eval* janet-val))
    ##
    :struct
    (make-hash-map (struct ;(map janet-eval* (kvs janet-val))))
    ##
    :table
    (make-hash-map (struct ;(map janet-eval* (kvs janet-val))))
    ##
    (throw* (make-string (string "Unsupported type: " (type janet-val))))))

(def mal-janet-eval
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "janet-eval requires 1 argument")))
      (let [head-ast (in asts 0)]
        (when (not (string?* head-ast))
          (throw* (make-string
                    "janet-eval first argument should be a string")))
        (let [res (try
                    (eval-string (head-ast :content)) # XXX: escaping?
                    ([err]
                     (throw* (make-string (string "Eval failed: " err)))))]
          (janet-eval* res))))))

##

(def mal-type
  (make-function
    (fn [asts]
      (when (< (length asts) 1)
        (throw* (make-string "type requires 1 argument")))
      (make-keyword ((in asts 0) :tag)))))

(def unimplemented mal-throw)

(def ns
  {(make-symbol "+") (arith-fn +)
   (make-symbol "-") (arith-fn -)
   (make-symbol "*") (arith-fn *)
   (make-symbol "/") (arith-fn /)
   (make-symbol "list") mal-list
   (make-symbol "list?") mal-list?
   (make-symbol "vec") mal-vec
   (make-symbol "vector?") mal-vector?
   (make-symbol "empty?") mal-empty?
   (make-symbol "count") mal-count
   (make-symbol "=") mal-=
   (make-symbol "<") (cmp-fn <)
   (make-symbol "<=") (cmp-fn <=)
   (make-symbol ">") (cmp-fn >)
   (make-symbol ">=") (cmp-fn >=)
   (make-symbol "pr-str") mal-pr-str
   (make-symbol "str") mal-str
   (make-symbol "prn") mal-prn
   (make-symbol "println") mal-println
   (make-symbol "read-string") mal-read-string
   (make-symbol "slurp") mal-slurp
   (make-symbol "atom") mal-atom
   (make-symbol "atom?") mal-atom?
   (make-symbol "deref") mal-deref
   (make-symbol "reset!") mal-reset!
   (make-symbol "swap!") mal-swap!
   (make-symbol "cons") mal-cons
   (make-symbol "concat") mal-concat
   (make-symbol "nth") mal-nth
   (make-symbol "first") mal-first
   (make-symbol "rest") mal-rest
   (make-symbol "throw") mal-throw
   (make-symbol "apply") mal-apply
   (make-symbol "map") mal-map
   (make-symbol "nil?") mal-nil?
   (make-symbol "true?") mal-true?
   (make-symbol "false?") mal-false?
   (make-symbol "symbol?") mal-symbol?
   (make-symbol "symbol") mal-symbol
   (make-symbol "keyword") mal-keyword
   (make-symbol "keyword?") mal-keyword?
   (make-symbol "vector") mal-vector
   (make-symbol "sequential?") mal-sequential?
   (make-symbol "hash-map") mal-hash-map
   (make-symbol "map?") mal-map?
   (make-symbol "assoc") mal-assoc
   (make-symbol "dissoc") mal-dissoc
   (make-symbol "get") mal-get
   (make-symbol "contains?") mal-contains?
   (make-symbol "keys") mal-keys
   (make-symbol "vals") mal-vals
   (make-symbol "readline") mal-readline
   (make-symbol "time-ms") mal-time-ms
   (make-symbol "meta") mal-meta
   (make-symbol "with-meta") mal-with-meta
   (make-symbol "fn?") mal-fn?
   (make-symbol "string?") mal-string?
   (make-symbol "number?") mal-number?
   (make-symbol "conj") mal-conj
   (make-symbol "seq") mal-seq
   (make-symbol "macro?") mal-macro?
   (make-symbol "janet-eval") mal-janet-eval
   ##
   (make-symbol "type") mal-type
})
