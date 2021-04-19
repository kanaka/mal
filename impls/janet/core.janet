(import ./types :as t)
(import ./utils :as u)
(import ./printer)
(import ./reader)

(defn deref*
  [ast]
  (if (not (t/atom?* ast))
    (u/throw* (t/make-string (string "Expected atom, got: " (t/get-type ast))))
    (t/get-value ast)))

(defn reset!*
  [atom-ast val-ast]
  (t/set-atom-value! atom-ast val-ast)
  val-ast)

(defn cons*
  [head-ast tail-ast]
  [head-ast ;(t/get-value tail-ast)])

(defn concat*
  [& list-asts]
  (reduce (fn [acc list-ast]
            [;acc ;(t/get-value list-ast)])
          []
          list-asts))

(defn nth*
  [coll-ast num-ast]
  (let [elts (t/get-value coll-ast)
        n-elts (length elts)
        i (t/get-value num-ast)]
    (if (< i n-elts)
      (in elts i)
      (u/throw* (t/make-string (string "Index out of range: " i))))))

(defn first*
  [coll-or-nil-ast]
  (if (or (t/nil?* coll-or-nil-ast)
          (t/empty?* coll-or-nil-ast))
    t/mal-nil
    (in (t/get-value coll-or-nil-ast) 0)))

(defn rest*
  [coll-or-nil-ast]
  (if (or (t/nil?* coll-or-nil-ast)
          (t/empty?* coll-or-nil-ast))
    (t/make-list [])
    (t/make-list (slice (t/get-value coll-or-nil-ast) 1))))

(defn janet-eval*
  [janet-val]
  (case (type janet-val)
    :nil
    t/mal-nil
    ##
    :boolean
    (t/make-boolean janet-val)
    ##
    :number # XXX: there may be some incompatibilities
    (t/make-number janet-val)
    ##
    :string
    (t/make-string janet-val)
    ##
    :keyword # XXX: there may be some incompatibilities
    (t/make-keyword (string ":" janet-val))
    ##
    :symbol # XXX: there may be some incompatibilities
    (t/make-symbol (string janet-val))
    ##
    :tuple
    (t/make-list (map janet-eval* janet-val))
    ##
    :array
    (t/make-list (map janet-eval* janet-val))
    ##
    :struct
    (t/make-hash-map (struct ;(map janet-eval* (kvs janet-val))))
    ##
    :table
    (t/make-hash-map (struct ;(map janet-eval* (kvs janet-val))))
    ##
    (u/throw* (t/make-string (string "Unsupported type: " (type janet-val))))))

(defn arith-fn
  [op]
  (t/make-function
    (fn [asts]
      (t/make-number
        (op ;(map |(t/get-value $)
                  asts))))))

(defn cmp-fn
  [op]
  (t/make-function
    (fn [asts]
      (if (op ;(map |(t/get-value $) asts))
        t/mal-true
        t/mal-false))))

(def mal-symbol
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "symbol requires 1 argument")))
      (t/make-symbol (t/get-value (in asts 0))))))

(def mal-keyword
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "keyword requires 1 argument")))
      (let [arg-ast (in asts 0)]
        (cond
          (t/keyword?* arg-ast)
          arg-ast
          ##
          (t/string?* arg-ast)
          (t/make-keyword (string ":" (t/get-value arg-ast)))
          ##
          (u/throw* (t/make-string "Expected string")))))))

(def mal-list
  (t/make-function
    (fn [asts]
      (t/make-list asts))))

(def mal-vector
  (t/make-function
    (fn [asts]
      (t/make-vector asts))))

(def mal-vec
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "vec requires 1 argument")))
      (let [ast (in asts 0)]
        (cond
          (t/vector?* ast)
          ast
          ##
          (t/list?* ast)
          (t/make-vector (t/get-value ast))
          ##
          (t/nil?* ast)
          (t/make-vector ())
          ##
          (u/throw* (t/make-string "vec requires a vector, list, or nil")))))))

(def mal-hash-map
  (t/make-function
    (fn [asts]
      (when (= 1 (% (length asts) 2))
        (u/throw* (t/make-string
                    "hash-map requires an even number of arguments")))
      (t/make-hash-map asts))))

(def mal-atom
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "atom requires 1 argument")))
      (t/make-atom (in asts 0)))))

(def mal-nil?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "nil? requires 1 argument")))
      (if (t/nil?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-true?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "true? requires 1 argument")))
      (if (t/true?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-false?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "false? requires 1 argument")))
      (if (t/false?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-number?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "number? requires 1 argument")))
      (if (t/number?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-symbol?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "symbol? requires 1 argument")))
      (if (t/symbol?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-keyword?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "keyword? requires 1 argument")))
      (if (t/keyword?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-string?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "string? requires 1 argument")))
      (if (t/string?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-list?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "list? requires 1 argument")))
      (if (t/list?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-vector?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "vector? requires 1 argument")))
      (if (t/vector?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-map?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "map? requires 1 argument")))
      (if (t/hash-map?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-fn?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "fn? requires 1 argument")))
      (let [target-ast (in asts 0)]
        (if (and (t/fn?* target-ast)
                 (not (t/get-is-macro target-ast)))
          t/mal-true
          t/mal-false)))))

(def mal-macro?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "macro? requires 1 argument")))
      (let [the-ast (in asts 0)]
        (if (t/macro?* the-ast)
          t/mal-true
          t/mal-false)))))

(def mal-atom?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "atom? requires 1 argument")))
      (if (t/atom?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-sequential?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "sequential? requires 1 argument")))
      (if (or (t/list?* (in asts 0))
              (t/vector?* (in asts 0)))
        t/mal-true
        t/mal-false))))

(def mal-=
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "= requires 2 arguments")))
      (let [ast-1 (in asts 0)
            ast-2 (in asts 1)]
        (if (t/equals?* ast-1 ast-2)
          t/mal-true
          t/mal-false)))))

(def mal-empty?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "empty? requires 1 argument")))
      (if (t/empty?* (in asts 0))
        t/mal-true
        t/mal-false))))

(def mal-contains?
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "contains? requires 2 arguments")))
      (let [head-ast (in asts 0)]
        (when (not (or (t/hash-map?* head-ast)
                       (t/nil?* head-ast)))
          (u/throw* (t/make-string
                      "contains? first argument should be a hash-map or nil")))
        (if (t/nil?* head-ast)
          t/mal-nil
          (let [item-struct (t/get-value head-ast)
                key-ast (in asts 1)]
            (if-let [val-ast (get item-struct key-ast)]
              t/mal-true
              t/mal-false)))))))

(def mal-deref
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "deref requires 1 argument")))
      (let [ast (in asts 0)]
        (deref* ast)))))

(def mal-reset!
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "reset! requires 2 arguments")))
      (let [atom-ast (in asts 0)
            val-ast (in asts 1)]
        (reset!* atom-ast val-ast)))))

(def mal-swap!
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "swap! requires at least 2 arguments")))
      (let [atom-ast (in asts 0)
            fn-ast (in asts 1)
            args-asts (slice asts 2)
            inner-ast (deref* atom-ast)]
        (reset!* atom-ast
                 ((t/get-value fn-ast) [inner-ast ;args-asts]))))))

(def mal-pr-str
  (t/make-function
    (fn [asts]
      (def buf @"")
      (when (> (length asts) 0)
        (each ast asts
              (buffer/push-string buf (printer/pr_str ast true))
              (buffer/push-string buf " "))
        # remove extra space at end
        (buffer/popn buf 1))
      (t/make-string (string buf)))))

(def mal-str
  (t/make-function
    (fn [asts]
      (def buf @"")
      (when (> (length asts) 0)
        (each ast asts
              (buffer/push-string buf (printer/pr_str ast false))))
      (t/make-string (string buf)))))

(def mal-prn
  (t/make-function
    (fn [asts]
      (def buf @"")
      (when (> (length asts) 0)
        (each ast asts
              (buffer/push-string buf (printer/pr_str ast true))
              (buffer/push-string buf " "))
        # remove extra space at end
        (buffer/popn buf 1))
      (print (string buf))
      t/mal-nil)))

(def mal-println
  (t/make-function
    (fn [asts]
      (def buf @"")
      (when (> (length asts) 0)
        (each ast asts
              (buffer/push-string buf (printer/pr_str ast false))
              (buffer/push-string buf " "))
        # remove extra space at end
        (buffer/popn buf 1))
      (print (string buf))
      t/mal-nil)))

(def mal-read-string
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "read-string requires 1 argument")))
      (if-let [res (reader/read_str (t/get-value (in asts 0)))]
        res
        (u/throw* (t/make-string "No code content"))))))

(def mal-slurp
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "slurp requires 1 argument")))
      (let [a-str (t/get-value (in asts 0))]
        (if (not (os/stat a-str))
          (u/throw* (string "File not found: " a-str))
          # XXX: escaping?
          (t/make-string (slurp a-str)))))))

(def mal-count
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "count requires 1 argument")))
      (let [ast (in asts 0)]
        (if (t/nil?* ast)
          (t/make-number 0)
          (t/make-number (length (t/get-value ast))))))))

(def mal-cons
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "cons requires 2 arguments")))
      (let [head-ast (in asts 0)
            tail-ast (in asts 1)]
        (t/make-list (cons* head-ast tail-ast))))))

(def mal-concat
  (t/make-function
    (fn [asts]
      (t/make-list (concat* ;asts)))))

(def mal-nth
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "nth requires 2 arguments")))
      (let [coll-ast (in asts 0)
            num-ast (in asts 1)]
        (nth* coll-ast num-ast)))))

(def mal-first
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "first requires 1 argument")))
      (let [coll-or-nil-ast (in asts 0)]
        (first* coll-or-nil-ast)))))

(def mal-rest
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "rest requires 1 argument")))
      (let [coll-or-nil-ast (in asts 0)]
        (rest* coll-or-nil-ast)))))

(def mal-assoc
  (t/make-function
    (fn [asts]
      (when (< (length asts) 3)
        (u/throw* (t/make-string "assoc requires at least 3 arguments")))
      (let [head-ast (in asts 0)]
        (when (not (or (t/hash-map?* head-ast)
                       (t/nil?* head-ast)))
          (u/throw* (t/make-string
                      "assoc first argument should be a hash-map or nil")))
        (if (t/nil?* head-ast)
          t/mal-nil
          (let [item-table (table ;(kvs (t/get-value head-ast)))
                kv-asts (slice asts 1 -1)]
            (each [key-ast val-ast] (partition 2 kv-asts)
                  (put item-table key-ast val-ast))
            (t/make-hash-map (table/to-struct item-table))))))))

(def mal-dissoc
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "dissoc requires at least 2 arguments")))
      (let [head-ast (in asts 0)]
        (when (not (or (t/hash-map?* head-ast)
                       (t/nil?* head-ast)))
          (u/throw* (t/make-string
                      "dissoc first argument should be a hash-map or nil")))
        (if (t/nil?* head-ast)
          t/mal-nil
          (let [item-table (table ;(kvs (t/get-value head-ast)))
                key-asts (slice asts 1 -1)]
            (each key-ast key-asts
                  (put item-table key-ast nil))
            (t/make-hash-map (table/to-struct item-table))))))))

(def mal-get
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "get requires 2 arguments")))
      (let [head-ast (in asts 0)]
        (when (not (or (t/hash-map?* head-ast)
                       (t/nil?* head-ast)))
          (u/throw* (t/make-string
                      "get first argument should be a hash-map or nil")))
        (if (t/nil?* head-ast)
          t/mal-nil
          (let [item-struct (t/get-value head-ast)
                key-ast (in asts 1)]
            (if-let [val-ast (get item-struct key-ast)]
              val-ast
              t/mal-nil)))))))

(def mal-keys
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "keys requires 1 argument")))
      (let [head-ast (in asts 0)]
        (when (not (or (t/hash-map?* head-ast)
                       (t/nil?* head-ast)))
          (u/throw* (t/make-string
                      "keys first argument should be a hash-map or nil")))
        (if (t/nil?* head-ast)
          t/mal-nil
          (let [item-struct (t/get-value head-ast)]
            (t/make-list (keys item-struct))))))))

(def mal-vals
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "vals requires 1 argument")))
      (let [head-ast (in asts 0)]
        (when (not (or (t/hash-map?* head-ast)
                       (t/nil?* head-ast)))
          (u/throw* (t/make-string
                      "vals first argument should be a hash-map or nil")))
        (if (t/nil?* head-ast)
          t/mal-nil
          (let [item-struct (t/get-value head-ast)]
            (t/make-list (values item-struct))))))))

(def mal-conj
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "conj requires at least 2 arguments")))
      (let [coll-ast (in asts 0)
            item-asts (slice asts 1)]
        (cond
          (t/nil?* coll-ast)
          (t/make-list [;(reverse item-asts)])
          ##
          (t/list?* coll-ast)
          (t/make-list [;(reverse item-asts) ;(t/get-value coll-ast)])
          ##
          (t/vector?* coll-ast)
          (t/make-vector [;(t/get-value coll-ast) ;item-asts])
          ##
          (u/throw* (t/make-string "Expected list or vector")))))))

(def mal-seq
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "seq requires 1 argument")))
      (let [arg-ast (in asts 0)]
        (cond
          (t/list?* arg-ast)
          (if (t/empty?* arg-ast)
            t/mal-nil
            arg-ast)
          ##
          (t/vector?* arg-ast)
          (if (t/empty?* arg-ast)
            t/mal-nil
            (t/make-list (t/get-value arg-ast)))
          ##
          (t/string?* arg-ast)
          (if (t/empty?* arg-ast)
            t/mal-nil
            (let [str-asts (map |(t/make-string (string/from-bytes $))
                                (t/get-value arg-ast))]
              (t/make-list str-asts)))
          ##
          (t/nil?* arg-ast)
          arg-ast
          ##
          (u/throw* (t/make-string "Expected list, vector, string, or nil")))))))

(def mal-map
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "map requires at least 2 arguments")))
      (let [the-fn (t/get-value (in asts 0))
            coll (t/get-value (in asts 1))]
        (t/make-list (map |(the-fn [$])
                        coll))))))

# (apply F A B [C D]) is equivalent to (F A B C D)
(def mal-apply
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "apply requires at least 1 argument")))
      (let [the-fn (t/get-value (in asts 0))] # e.g. F
        (if (= (length asts) 1)
          (the-fn [])
          (let [last-asts (t/get-value (get (slice asts -2) 0)) # e.g. [C D]
                args-asts (slice asts 1 -2)] # e.g. [A B]
            (the-fn [;args-asts ;last-asts])))))))

(def mal-meta
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "meta requires 1 argument")))
      (let [head-ast (in asts 0)]
        (if (or (t/list?* head-ast)
                (t/vector?* head-ast)
                (t/hash-map?* head-ast)
                (t/fn?* head-ast))
          (t/get-meta (in asts 0))
          t/mal-nil)))))

(def mal-with-meta
  (t/make-function
    (fn [asts]
      (when (< (length asts) 2)
        (u/throw* (t/make-string "with-meta requires 2 arguments")))
      (let [target-ast (in asts 0)
            meta-ast (in asts 1)]
        (cond
          (t/list?* target-ast)
          (t/make-list (t/get-value target-ast) meta-ast)
          ##
          (t/vector?* target-ast)
          (t/make-vector (t/get-value target-ast) meta-ast)
          ##
          (t/hash-map?* target-ast)
          (t/make-hash-map (t/get-value target-ast) meta-ast)
          ##
          (t/fn?* target-ast)
          (t/clone-with-meta target-ast meta-ast)
          ##
          (u/throw* (t/make-string "Expected list, vector, hash-map, or fn")))))))

(def mal-throw
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "throw requires 1 argument")))
      (u/throw* (in asts 0)))))

(def mal-readline
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "readline requires 1 argument")))
      (let [prompt (t/get-value (in asts 0))
            buf @""]
        (file/write stdout prompt)
        (file/flush stdout)
        (file/read stdin :line buf)
        (if (< 0 (length buf))
          (t/make-string (string/trimr buf))
          t/mal-nil)))))

(def mal-time-ms
  (t/make-function
    (fn [asts]
      (t/make-number
        (math/floor (* 1000 (os/clock)))))))

(def mal-janet-eval
  (t/make-function
    (fn [asts]
      (when (< (length asts) 1)
        (u/throw* (t/make-string "janet-eval requires 1 argument")))
      (let [head-ast (in asts 0)]
        (when (not (t/string?* head-ast))
          (u/throw* (t/make-string
                      "janet-eval first argument should be a string")))
        (let [res (try
                    (eval-string (t/get-value head-ast)) # XXX: escaping?
                    ([err]
                     (u/throw* (t/make-string (string "Eval failed: " err)))))]
          (janet-eval* res))))))

(def unimplemented mal-throw)

(def ns
  {(t/make-symbol "+") (arith-fn +)
   (t/make-symbol "-") (arith-fn -)
   (t/make-symbol "*") (arith-fn *)
   (t/make-symbol "/") (arith-fn /)
   (t/make-symbol "list") mal-list
   (t/make-symbol "list?") mal-list?
   (t/make-symbol "vec") mal-vec
   (t/make-symbol "vector?") mal-vector?
   (t/make-symbol "empty?") mal-empty?
   (t/make-symbol "count") mal-count
   (t/make-symbol "=") mal-=
   (t/make-symbol "<") (cmp-fn <)
   (t/make-symbol "<=") (cmp-fn <=)
   (t/make-symbol ">") (cmp-fn >)
   (t/make-symbol ">=") (cmp-fn >=)
   (t/make-symbol "pr-str") mal-pr-str
   (t/make-symbol "str") mal-str
   (t/make-symbol "prn") mal-prn
   (t/make-symbol "println") mal-println
   (t/make-symbol "read-string") mal-read-string
   (t/make-symbol "slurp") mal-slurp
   (t/make-symbol "atom") mal-atom
   (t/make-symbol "atom?") mal-atom?
   (t/make-symbol "deref") mal-deref
   (t/make-symbol "reset!") mal-reset!
   (t/make-symbol "swap!") mal-swap!
   (t/make-symbol "cons") mal-cons
   (t/make-symbol "concat") mal-concat
   (t/make-symbol "nth") mal-nth
   (t/make-symbol "first") mal-first
   (t/make-symbol "rest") mal-rest
   (t/make-symbol "throw") mal-throw
   (t/make-symbol "apply") mal-apply
   (t/make-symbol "map") mal-map
   (t/make-symbol "nil?") mal-nil?
   (t/make-symbol "true?") mal-true?
   (t/make-symbol "false?") mal-false?
   (t/make-symbol "symbol?") mal-symbol?
   (t/make-symbol "symbol") mal-symbol
   (t/make-symbol "keyword") mal-keyword
   (t/make-symbol "keyword?") mal-keyword?
   (t/make-symbol "vector") mal-vector
   (t/make-symbol "sequential?") mal-sequential?
   (t/make-symbol "hash-map") mal-hash-map
   (t/make-symbol "map?") mal-map?
   (t/make-symbol "assoc") mal-assoc
   (t/make-symbol "dissoc") mal-dissoc
   (t/make-symbol "get") mal-get
   (t/make-symbol "contains?") mal-contains?
   (t/make-symbol "keys") mal-keys
   (t/make-symbol "vals") mal-vals
   (t/make-symbol "readline") mal-readline
   (t/make-symbol "time-ms") mal-time-ms
   (t/make-symbol "meta") mal-meta
   (t/make-symbol "with-meta") mal-with-meta
   (t/make-symbol "fn?") mal-fn?
   (t/make-symbol "string?") mal-string?
   (t/make-symbol "number?") mal-number?
   (t/make-symbol "conj") mal-conj
   (t/make-symbol "seq") mal-seq
   (t/make-symbol "macro?") mal-macro?
   (t/make-symbol "janet-eval") mal-janet-eval
})
