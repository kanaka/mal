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
      (let [ast-1 (in asts 0)
            ast-2 (in asts 1)]
        (make-number (string (op (scan-number (ast-1 :content))
                                 (scan-number (ast-2 :content)))))))))

(def create-list
  (make-function
    (fn [asts]
      (make-list asts))))

(defn mal-list?*
  [ast]
  (= (ast :tag) :list))

(def mal-list?
  (make-function
    (fn [asts]
      (if (mal-list?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn mal-vector?*
  [ast]
  (= (ast :tag) :vector))

(def create-vector
  (make-function
    (fn [asts]
      (let [ast (in asts 0)]
        (if (mal-vector?* ast)
          ast
          (make-vector (ast :content)))))))

(def mal-vector?
  (make-function
    (fn [asts]
      (if (mal-vector?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn is-empty?*
  [ast]
  (empty? (ast :content)))

(def is-empty?
  (make-function
    (fn [asts]
      (let [ast (in asts 0)]
        (if (is-empty?* ast)
          (make-boolean true)
          (make-boolean false))))))

(def count-elts
  (make-function
    (fn [asts]
      (let [ast (in asts 0)]
        (let [tag (ast :tag)]
          (if (= tag :nil)
            (make-number "0")
            (make-number (string (length (ast :content))))))))))


(defn cmp-fn
  [op]
  (make-function
    (fn [asts]
      (let [ast-1 (in asts 0)
            ast-2 (in asts 1)]
        (let [val-1 (scan-number (ast-1 :content))
              val-2 (scan-number (ast-2 :content))]
          (if (op val-1 val-2)
            (make-boolean true)
            (make-boolean false)))))))

(def pr-str
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

(def str
  (make-function
    (fn [asts]
      (def buf @"")
      (when (> (length asts) 0)
        (each ast asts
              (buffer/push-string buf (printer/pr_str ast false))))
      (make-string (string buf)))))

(def prn
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

(def println
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

(def read-string
  (make-function
    (fn [asts]
      (reader/read_str ((in asts 0) :content)))))

(def mal-slurp
  (make-function
    (fn [asts]
      (let [a-str ((in asts 0) :content)]
        (if (not (os/stat a-str))
          (throw* (string "File not found: " a-str))
          # XXX: escaping?
          (make-string (slurp a-str)))))))

(def create-atom
  (make-function
    (fn [asts]
      (make-atom (in asts 0)))))

(defn mal-atom?*
  [ast]
  (= (ast :tag) :atom))

(def mal-atom?
  (make-function
    (fn [asts]
      (if (mal-atom?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn deref*
  [ast]
  (if (not (mal-atom?* ast))
    (throw* (make-string (string "Expected atom, got: " (ast :tag))))
    (ast :content)))

(def deref
  (make-function
    (fn [asts]
      (let [ast (in asts 0)]
        (deref* ast)))))

(defn reset!*
  [atom-ast val-ast]
  (put atom-ast
       :content val-ast)
  val-ast)

(def reset!
  (make-function
    (fn [asts]
      (let [atom-ast (in asts 0)
            val-ast (in asts 1)]
        (reset!* atom-ast val-ast)))))

(def swap!
  (make-function
    (fn [asts]
      (let [atom-ast (in asts 0)
            fn-ast (in asts 1)
            args-asts (slice asts 2)
            inner-ast (deref* atom-ast)]
        (reset!* atom-ast
                 ((fn-ast :content) [inner-ast ;args-asts]))))))

(defn cons*
  [head-ast tail-ast]
  [head-ast ;(tail-ast :content)])

(def cons
  (make-function
    (fn [asts]
      (let [head-ast (in asts 0)
            tail-ast (in asts 1)]
        (make-list (cons* head-ast tail-ast))))))

(defn concat*
  [& list-asts]
  (reduce (fn [acc list-ast]
            [;acc ;(list-ast :content)])
          []
          list-asts))

(def concat
  (make-function
    (fn [asts]
      (make-list (concat* ;asts)))))

(defn starts-with
  [ast name]
  (when (and (not (is-empty?* ast))
             (mal-list?* ast))
    (let [head-ast (in (ast :content) 0)]
      (and (= :symbol (head-ast :tag))
           (= name (head-ast :content))))))

(var quasiquote* nil)

(defn qq-iter
  [ast]
  (if (is-empty?* ast)
    (make-list ())
    (let [elt (in (ast :content) 0)
          acc (qq-iter (make-list (slice (ast :content) 1)))]
      (if (starts-with elt "splice-unquote")
        (make-list [(make-symbol "concat")
                    (in (elt :content) 1)
                    acc])
        (make-list [(make-symbol "cons")
                    (quasiquote* elt)
                    acc])))))

(varfn quasiquote*
  [ast]
  (cond
    (starts-with ast "unquote")
    (in (ast :content) 1)
    ##
    (mal-list?* ast)
    (qq-iter ast)
    ##
    (mal-vector?* ast)
    (make-list [(make-symbol "vec") (qq-iter ast)])
    ##
    (or (= :symbol (ast :tag))
        (= :hash-map (ast :tag)))
    (make-list [(make-symbol "quote") ast])
    ##
    ast))

(defn nth*
  [coll-ast num-ast]
  (let [elts (coll-ast :content)
        n-elts (length elts)
        i (scan-number (num-ast :content))]
    (if (< i n-elts)
      (in elts i)
      (throw* (make-string (string "Index out of range: " i))))))

(def nth
  (make-function
    (fn [asts]
      (let [coll-ast (in asts 0)
            num-ast (in asts 1)]
        (nth* coll-ast num-ast)))))

(defn first*
  [coll-or-nil-ast]
  (if (or (= (coll-or-nil-ast :tag) :nil)
          (is-empty?* coll-or-nil-ast))
    (make-nil)
    (in (coll-or-nil-ast :content) 0)))

(def mal-first
  (make-function
    (fn [asts]
      (let [coll-or-nil-ast (in asts 0)]
        (first* coll-or-nil-ast)))))

(defn rest*
  [coll-or-nil-ast]
  (if (or (= (coll-or-nil-ast :tag) :nil)
          (is-empty?* coll-or-nil-ast))
    (make-list [])
    (make-list (slice (coll-or-nil-ast :content) 1))))

(def rest
  (make-function
    (fn [asts]
      (let [coll-or-nil-ast (in asts 0)]
        (rest* coll-or-nil-ast)))))

(def throw
  (make-function
    (fn [asts]
      (throw* (in asts 0)))))

# (apply F A B [C D]) is equivalent to (F A B C D)
(def mal-apply
  (make-function
    (fn [asts]
      (assert (< 1 (length asts))
              "apply requires at least 2 arguments")
      (let [the-fn ((in asts 0) :content) # r.g. F
            last-asts ((get (slice asts -2) 0) :content) # e.g. [C D]
            args-asts (slice asts 1 -2)] # e.g. [A B]
        (the-fn [;args-asts ;last-asts])))))

(def mal-map
  (make-function
    (fn [asts]
      (assert (< 1 (length asts))
              "map requires at least 2 arguments")
      (let [the-fn ((in asts 0) :content)
            coll ((in asts 1) :content)]
        (make-list (map |(the-fn [$])
                        coll))))))

(defn nil?*
  [ast]
  (= :nil (ast :tag)))

(def mal-nil?
  (make-function
    (fn [asts]
      (if (nil?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn true?*
  [ast]
  (and (= :boolean (ast :tag))
       (= "true" (ast :content))))

(def mal-true?
  (make-function
    (fn [asts]
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
      (if (false?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn symbol?*
  [ast]
  (= :symbol (ast :tag)))

(def mal-symbol?
  (make-function
    (fn [asts]
      (if (symbol?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(def mal-type
  (make-function
    (fn [asts]
      (make-keyword ((in asts 0) :tag)))))

(def create-symbol
  (make-function
    (fn [asts]
      (make-symbol ((in asts 0) :content)))))

(defn mal-keyword?*
  [ast]
  (= (ast :tag) :keyword))

(defn mal-string?*
  [ast]
  (= (ast :tag) :string))

(def create-keyword
  (make-function
    (fn [asts]
      (let [arg-ast (in asts 0)]
        (cond
          (mal-keyword?* arg-ast)
          arg-ast
          ##
          (mal-string?* arg-ast)
          (make-keyword (string ":" (arg-ast :content)))
          ##
          (throw* (make-string "Expected string")))))))

(def mal-keyword?
  (make-function
    (fn [asts]
      (if (mal-keyword?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(def create-vector-from-items
  (make-function
    (fn [asts]
      (make-vector asts))))

(def mal-sequential?
  (make-function
    (fn [asts]
      (if (or (mal-list?* (in asts 0))
              (mal-vector?* (in asts 0)))
        (make-boolean true)
        (make-boolean false)))))

(def create-hash-map
  (make-function
    (fn [asts]
      (make-hash-map asts))))

(defn mal-hash-map?*
  [ast]
  (= (ast :tag) :hash-map))

(def mal-map?
  (make-function
    (fn [asts]
      (if (mal-hash-map?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(def assoc
  (make-function
    (fn [asts]
      (let [item-table (table ;(kvs ((in asts 0) :content)))
            kv-asts (slice asts 1 -1)]
        (each [key-ast val-ast] (partition 2 kv-asts)
          (put item-table key-ast val-ast))
        (make-hash-map (table/to-struct item-table))))))

(def dissoc
  (make-function
    (fn [asts]
      (let [item-table (table ;(kvs ((in asts 0) :content)))
            key-asts (slice asts 1 -1)]
        (each key-ast key-asts
              (put item-table key-ast nil))
        (make-hash-map (table/to-struct item-table))))))

(def mal-get
  (make-function
    (fn [asts]
      (let [item-struct ((in asts 0) :content)
            key-ast (in asts 1)]
        (if-let [val-ast (get item-struct key-ast)]
          val-ast
          (make-nil))))))

(def contains?
  (make-function
    (fn [asts]
      (let [item-struct ((in asts 0) :content)
            key-ast (in asts 1)]
        (if-let [val-ast (get item-struct key-ast)]
          (make-boolean true)
          (make-boolean false))))))

(def mal-keys
  (make-function
    (fn [asts]
      (let [item-struct ((in asts 0) :content)]
        (make-list (keys item-struct))))))

(def mal-vals
  (make-function
    (fn [asts]
      (let [item-struct ((in asts 0) :content)]
        (make-list (values item-struct))))))

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
        (if (and (not (mal-list?* ast-1))
                 (not (mal-vector?* ast-1))
                 (not (mal-hash-map?* ast-1)))
          (= val-1 val-2)
          (if (not= (length val-1) (length val-2))
            false
            (if (and (not (mal-hash-map?* ast-1))
                     (not (mal-hash-map?* ast-2)))
              (do
                (var found-unequal false)
                (each [v1 v2] (partition 2 (interleave val-1 val-2))
                      (when (not (equals?* v1 v2))
                        (set found-unequal true)
                        (break)))
                (not found-unequal))
              (if (or (not (mal-hash-map?* ast-1))
                      (not (mal-hash-map?* ast-2)))
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

(def equals?
  (make-function
    (fn [asts]
      (let [ast-1 (in asts 0)
            ast-2 (in asts 1)]
        (if (equals?* ast-1 ast-2)
          (make-boolean true)
          (make-boolean false))))))

(def ns
  {(make-symbol "+") (arith-fn +)
   (make-symbol "-") (arith-fn -)
   (make-symbol "*") (arith-fn *)
   (make-symbol "/") (arith-fn /)
   (make-symbol "list") create-list
   (make-symbol "list?") mal-list?
   (make-symbol "vec") create-vector
   (make-symbol "vector?") mal-vector?
   (make-symbol "empty?") is-empty?
   (make-symbol "count") count-elts
   (make-symbol "=") equals?
   (make-symbol "<") (cmp-fn <)
   (make-symbol "<=") (cmp-fn <=)
   (make-symbol ">") (cmp-fn >)
   (make-symbol ">=") (cmp-fn >=)
   (make-symbol "pr-str") pr-str
   (make-symbol "str") str
   (make-symbol "prn") prn
   (make-symbol "println") println
   (make-symbol "read-string") read-string
   (make-symbol "slurp") mal-slurp
   (make-symbol "atom") create-atom
   (make-symbol "atom?") mal-atom?
   (make-symbol "deref") deref
   (make-symbol "reset!") reset!
   (make-symbol "swap!") swap!
   (make-symbol "cons") cons
   (make-symbol "concat") concat
   (make-symbol "nth") nth
   (make-symbol "first") mal-first
   (make-symbol "rest") rest
   (make-symbol "throw") throw
   (make-symbol "apply") mal-apply
   (make-symbol "map") mal-map
   (make-symbol "nil?") mal-nil?
   (make-symbol "true?") mal-true?
   (make-symbol "false?") mal-false?
   (make-symbol "symbol?") mal-symbol?
   (make-symbol "symbol") create-symbol
   (make-symbol "keyword") create-keyword
   (make-symbol "keyword?") mal-keyword?
   (make-symbol "vector") create-vector-from-items
   (make-symbol "sequential?") mal-sequential?
   (make-symbol "hash-map") create-hash-map
   (make-symbol "map?") mal-map?
   (make-symbol "assoc") assoc
   (make-symbol "dissoc") dissoc
   (make-symbol "get") mal-get
   (make-symbol "contains?") contains?
   (make-symbol "keys") mal-keys
   (make-symbol "vals") mal-vals
   ##
   (make-symbol "type") mal-type
})
