(local t (require :types))
(local u (require :utils))
(local printer (require :printer))
(local reader (require :reader))
(local fennel (require :fennel))

(local mal-list
  (t.make-fn
    (fn [asts]
      (t.make-list asts))))

(local mal-list?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "list? takes 1 argument")))
      (t.make-boolean (t.list?* (. asts 1))))))

(local mal-empty?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "empty? takes 1 argument")))
      (let [arg-ast (. asts 1)]
        (if (t.nil?* arg-ast)
            t.mal-true
            (t.make-boolean (t.empty?* arg-ast)))))))

(local mal-count
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "count takes 1 argument")))
      (let [arg-ast (. asts 1)]
        (if (t.nil?* arg-ast)
            (t.make-number 0)
            (t.make-number (length (t.get-value arg-ast))))))))

(local mal-=
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "= takes 2 arguments")))
      (let [ast-1 (. asts 1)
            ast-2 (. asts 2)]
        (if (t.equals?* ast-1 ast-2)
            t.mal-true
            t.mal-false)))))

(local mal-pr-str
  (t.make-fn
    (fn [asts]
      (local buf [])
      (when (> (length asts) 0)
        (each [i ast (ipairs asts)]
          (table.insert buf (printer.pr_str ast true))
          (table.insert buf " "))
        ;; remove extra space at end
        (table.remove buf))
      (t.make-string (table.concat buf)))))

(local mal-str
  (t.make-fn
    (fn [asts]
      (local buf [])
      (when (> (length asts) 0)
        (each [i ast (ipairs asts)]
          (table.insert buf (printer.pr_str ast false))))
      (t.make-string (table.concat buf)))))

(local mal-prn
  (t.make-fn
    (fn [asts]
      (local buf [])
      (when (> (length asts) 0)
        (each [i ast (ipairs asts)]
          (table.insert buf (printer.pr_str ast true))
          (table.insert buf " "))
        ;; remove extra space at end
        (table.remove buf))
      (print (table.concat buf))
      t.mal-nil)))

(local mal-println
  (t.make-fn
    (fn [asts]
      (local buf [])
      (when (> (length asts) 0)
        (each [i ast (ipairs asts)]
          (table.insert buf (printer.pr_str ast false))
          (table.insert buf " "))
        ;; remove extra space at end
        (table.remove buf))
      (print (table.concat buf))
      t.mal-nil)))

(local mal-read-string
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "read-string takes 1 argument")))
      (let [res (reader.read_str (t.get-value (. asts 1)))]
        (if res
            res
            (u.throw* (t.make-string "No code content")))))))

(local mal-slurp
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "slurp takes 1 argument")))
      (let [a-str (t.get-value (. asts 1))]
        ;; XXX: error handling?
        (with-open [f (io.open a-str)]
          ;; XXX: escaping?
          (t.make-string (f:read "*a")))))))

(local mal-atom
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "atom takes 1 argument")))
      (t.make-atom (. asts 1)))))

(local mal-atom?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "atom? takes 1 argument")))
      (if (t.atom?* (. asts 1))
        t.mal-true
        t.mal-false))))

(local mal-deref
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "deref takes 1 argument")))
      (let [ast (. asts 1)]
        (t.deref* ast)))))

(local mal-reset!
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "reset! takes 2 arguments")))
      (let [atom-ast (. asts 1)
            val-ast (. asts 2)]
        (t.reset!* atom-ast val-ast)))))

(local mal-swap!
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "swap! takes at least 2 arguments")))
      (let [atom-ast (. asts 1)
            fn-ast (. asts 2)
            args-asts (u.slice asts 3 -1)
            args-tbl [(t.deref* atom-ast) (table.unpack args-asts)]]
        (t.reset!* atom-ast
                   ((t.get-value fn-ast) args-tbl))))))

(local mal-cons
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "cons takes 2 arguments")))
      (let [head-ast (. asts 1)
            tail-ast (. asts 2)]
        (t.make-list [head-ast
                      (table.unpack (t.get-value tail-ast))])))))

(local mal-concat
  (t.make-fn
    (fn [asts]
      (local acc [])
      (for [i 1 (length asts)]
         (each [j elt (ipairs (t.get-value (. asts i)))]
           (table.insert acc elt)))
      (t.make-list acc))))

(local mal-vec
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "vec takes 1 argument")))
      (let [ast (. asts 1)]
        (if (t.vector?* ast)
            ast
            ;;
            (t.list?* ast)
            (t.make-vector (t.get-value ast))
            ;;
            (t.nil?* ast)
            (t.make-vector [])
            ;;
            (u.throw* (t.make-string "vec takes a vector, list, or nil")))))))

(local mal-nth
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "nth takes 2 arguments")))
      (let [elts (t.get-value (. asts 1))
            i (t.get-value (. asts 2))]
        (if (< i (length elts))
            (. elts (+ i 1))
            (u.throw* (t.make-string (.. "Index out of range: " i))))))))

(local mal-first
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "first takes 1 argument")))
      (let [coll-or-nil-ast (. asts 1)]
        (if (or (t.nil?* coll-or-nil-ast)
                (t.empty?* coll-or-nil-ast))
            t.mal-nil
            (. (t.get-value coll-or-nil-ast) 1))))))

(local mal-rest
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "rest takes 1 argument")))
      (let [coll-or-nil-ast (. asts 1)]
        (if (or (t.nil?* coll-or-nil-ast)
                (t.empty?* coll-or-nil-ast))
            (t.make-list [])
            (t.make-list (u.slice (t.get-value coll-or-nil-ast) 2 -1)))))))

(local mal-throw
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "throw takes 1 argument")))
      (u.throw* (. asts 1)))))

;; (apply F A B [C D]) is equivalent to (F A B C D)
(local mal-apply
  (t.make-fn
    (fn [asts]
      (let [n-asts (length asts)]
        (when (< n-asts 1)
          (u.throw* (t.make-string "apply takes at least 1 argument")))
        (let [the-fn (t.get-value (. asts 1))] ; e.g. F
          (if (= n-asts 1)
              (the-fn [])
              (= n-asts 2)
              (the-fn [(table.unpack (t.get-value (. asts 2)))])
              (let [args-asts (u.slice asts 2 -2) ; e.g. [A B]
                    last-asts (t.get-value (u.last asts)) ; e.g. [C D]
                    fn-args-tbl []]
                (each [i elt (ipairs args-asts)]
                  (table.insert fn-args-tbl elt))
                (each [i elt (ipairs last-asts)]
                  (table.insert fn-args-tbl elt))
                (the-fn fn-args-tbl))))))))

(local mal-map
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "map takes at least 2 arguments")))
      (let [the-fn (t.get-value (. asts 1))
            coll (t.get-value (. asts 2))]
        (t.make-list (u.map #(the-fn [$]) coll))))))

(local mal-nil?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "nil? takes 1 argument")))
      (if (t.nil?* (. asts 1))
          t.mal-true
          t.mal-false))))

(local mal-true?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "true? takes 1 argument")))
      (if (t.true?* (. asts 1))
        t.mal-true
        t.mal-false))))

(local mal-false?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "false? takes 1 argument")))
      (if (t.false?* (. asts 1))
        t.mal-true
        t.mal-false))))

(local mal-symbol?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "symbol? takes 1 argument")))
      (if (t.symbol?* (. asts 1))
        t.mal-true
        t.mal-false))))

(local mal-symbol
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "symbol takes 1 argument")))
      ;; XXX: check that type is string?
      (t.make-symbol (t.get-value (. asts 1))))))

(local mal-keyword
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "keyword takes 1 argument")))
      (let [arg-ast (. asts 1)]
        (if (t.keyword?* arg-ast)
            arg-ast
            ;;
            (t.string?* arg-ast)
            (t.make-keyword (.. ":" (t.get-value arg-ast)))
            ;;
            (u.throw* (t.make-string "Expected string")))))))

(local mal-keyword?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "keyword? takes 1 argument")))
      (if (t.keyword?* (. asts 1))
          t.mal-true
          t.mal-false))))

(local mal-vector
  (t.make-fn
    (fn [asts]
      (t.make-vector asts))))

(local mal-vector?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "vector? takes 1 argument")))
      (if (t.vector?* (. asts 1))
          t.mal-true
          t.mal-false))))

(local mal-sequential?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "sequential? takes 1 argument")))
      (if (or (t.list?* (. asts 1))
              (t.vector?* (. asts 1)))
          t.mal-true
          t.mal-false))))

(local mal-map?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "map? takes 1 argument")))
      (if (t.hash-map?* (. asts 1))
          t.mal-true
          t.mal-false))))

(local mal-hash-map
  (t.make-fn
    (fn [asts]
      (when (= 1 (% (length asts) 2))
        (u.throw* (t.make-string
                   "hash-map takes an even number of arguments")))
      (t.make-hash-map asts))))

(local mal-assoc
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 3)
        (u.throw* (t.make-string "assoc takes at least 3 arguments")))
      (let [head-ast (. asts 1)]
        (when (not (or (t.hash-map?* head-ast)
                       (t.nil?* head-ast)))
          (u.throw* (t.make-string
                     "assoc first argument should be a hash-map or nil")))
        (if (t.nil?* head-ast)
            t.mal-nil
            (let [item-tbl []
                  kv-asts (u.slice asts 2 -1)
                  hash-items (t.get-value head-ast)]
              (for [i 1 (/ (length hash-items) 2)]
                (let [key (. hash-items (- (* 2 i) 1))]
                  (var idx 1)
                  (var found false)
                  (while (and (not found)
                              (<= idx (length kv-asts)))
                    (if (t.equals?* key (. kv-asts idx))
                        (set found true)
                        (set idx (+ idx 2))))
                  (if (not found)
                      (do
                       (table.insert item-tbl key)
                       (table.insert item-tbl (. hash-items (* 2 i))))
                      (do
                       (table.insert item-tbl key)
                       (table.insert item-tbl (. kv-asts (+ idx 1)))
                       (table.remove kv-asts (+ idx 1))
                       (table.remove kv-asts idx)))))
              (each [i elt (ipairs kv-asts)]
                (table.insert item-tbl elt))
              (t.make-hash-map item-tbl)))))))

(local mal-dissoc
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "dissoc takes at least 2 arguments")))
      (let [head-ast (. asts 1)]
        (when (not (or (t.hash-map?* head-ast)
                       (t.nil?* head-ast)))
          (u.throw* (t.make-string
                     "dissoc first argument should be a hash-map or nil")))
        (if (t.nil?* head-ast)
            t.mal-nil
            (let [item-tbl []
                  key-asts (u.slice asts 2 -1)
                  hash-items (t.get-value head-ast)]
              (for [i 1 (/ (length hash-items) 2)]
                (let [key (. hash-items (- (* 2 i) 1))]
                  (var idx 1)
                  (var found false)
                  (while (and (not found)
                              (<= idx (length key-asts)))
                    (if (t.equals?* key (. key-asts idx))
                        (set found true)
                        (set idx (+ idx 1))))
                  (when (not found)
                    (table.insert item-tbl key)
                    (table.insert item-tbl (. hash-items (* 2 i))))))
              (t.make-hash-map item-tbl)))))))

(local mal-get
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "get takes 2 arguments")))
      (let [head-ast (. asts 1)]
        (when (not (or (t.hash-map?* head-ast)
                       (t.nil?* head-ast)))
          (u.throw* (t.make-string
                     "get first argument should be a hash-map or nil")))
        (if (t.nil?* head-ast)
            t.mal-nil
            (let [hash-items (t.get-value head-ast)
                  key-ast (. asts 2)]
              (var idx 1)
              (var found false)
              (while (and (not found)
                          (<= idx (length hash-items)))
                (if (t.equals?* key-ast (. hash-items idx))
                    (set found true)
                    (set idx (+ idx 1))))
              (if found
                  (. hash-items (+ idx 1))
                  t.mal-nil)))))))

(local mal-contains?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "contains? takes 2 arguments")))
      (let [head-ast (. asts 1)]
        (when (not (or (t.hash-map?* head-ast)
                       (t.nil?* head-ast)))
          (u.throw* (t.make-string
                     "contains? first argument should be a hash-map or nil")))
        (if (t.nil?* head-ast)
            t.mal-nil
            (let [hash-items (t.get-value head-ast)
                  key-ast (. asts 2)]
              (var idx 1)
              (var found false)
              (while (and (not found)
                          (<= idx (length hash-items)))
                (if (t.equals?* key-ast (. hash-items idx))
                    (set found true)
                    (set idx (+ idx 1))))
              (if found
                  t.mal-true
                  t.mal-false)))))))

(local mal-keys
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "keys takes 1 argument")))
      (let [head-ast (. asts 1)]
        (when (not (or (t.hash-map?* head-ast)
                       (t.nil?* head-ast)))
          (u.throw* (t.make-string
                     "keys first argument should be a hash-map or nil")))
        (if (t.nil?* head-ast)
            t.mal-nil
            (let [item-tbl []
                  hash-items (t.get-value head-ast)]
              (for [i 1 (/ (length hash-items) 2)]
                (let [key (. hash-items (- (* 2 i) 1))]
                  (table.insert item-tbl key)))
              (t.make-list item-tbl)))))))

(local mal-vals
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "vals takes 1 argument")))
      (let [head-ast (. asts 1)]
        (when (not (or (t.hash-map?* head-ast)
                       (t.nil?* head-ast)))
          (u.throw* (t.make-string
                     "vals first argument should be a hash-map or nil")))
        (if (t.nil?* head-ast)
            t.mal-nil
            (let [item-tbl []
                  hash-items (t.get-value head-ast)]
              (for [i 1 (/ (length hash-items) 2)]
                (let [value (. hash-items (* 2 i))]
                  (table.insert item-tbl value)))
              (t.make-list item-tbl)))))))

(local mal-readline
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "vals takes 1 argument")))
      (let [prompt (t.get-value (. asts 1))]
        (io.write prompt)
        (io.flush)
        (let [input (io.read)
              trimmed (string.match input "^%s*(.-)%s*$")]
          (if (> (length trimmed) 0)
              (t.make-string trimmed)
              t.mal-nil))))))

(local mal-meta
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "meta takes 1 argument")))
      (let [head-ast (. asts 1)]
        (if (or (t.list?* head-ast)
                (t.vector?* head-ast)
                (t.hash-map?* head-ast)
                (t.fn?* head-ast))
            (t.get-md head-ast)
            t.mal-nil)))))

(local mal-with-meta
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "with-meta takes 2 arguments")))
      (let [target-ast (. asts 1)
            meta-ast (. asts 2)]
        (if (t.list?* target-ast)
            (t.make-list (t.get-value target-ast) meta-ast)
            ;;
            (t.vector?* target-ast)
            (t.make-vector (t.get-value target-ast) meta-ast)
            ;;
            (t.hash-map?* target-ast)
            (t.make-hash-map (t.get-value target-ast) meta-ast)
            ;;
            (t.fn?* target-ast)
            (t.clone-with-meta target-ast meta-ast)
            ;;
            (u.throw*
             (t.make-string "Expected list, vector, hash-map, or fn")))))))

(local mal-string?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "string? takes 1 argument")))
      (t.make-boolean (t.string?* (. asts 1))))))

(local mal-number?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "number? takes 1 argument")))
      (t.make-boolean (t.number?* (. asts 1))))))

(local mal-fn?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "fn? takes 1 argument")))
      (let [target-ast (. asts 1)]
        (if (and (t.fn?* target-ast)
                 (not (t.get-is-macro target-ast)))
            t.mal-true
            t.mal-false)))))

(local mal-macro?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "macro? requires 1 argument")))
      (let [the-ast (. asts 1)]
        (if (t.macro?* the-ast)
            t.mal-true
            t.mal-false)))))

(local mal-conj
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "conj takes at least 2 arguments")))
      (let [coll-ast (. asts 1)
            item-asts (u.slice asts 2 -1)]
        (if (t.nil?* coll-ast)
            (t.make-list (u.reverse item-asts))
            ;;
            (t.list?* coll-ast)
            (t.make-list (u.concat-two (u.reverse item-asts)
                                       (t.get-value coll-ast)))
            ;;
            (t.vector?* coll-ast)
            (t.make-vector (u.concat-two (t.get-value coll-ast)
                                         item-asts))
            ;;
            (u.throw* (t.make-string "Expected list, vector, or nil")))))))

(local mal-seq
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "seq takes 1 argument")))
      (let [arg-ast (. asts 1)]
        (if (t.list?* arg-ast)
            (if (t.empty?* arg-ast)
                t.mal-nil
                arg-ast)
            ;;
            (t.vector?* arg-ast)
            (if (t.empty?* arg-ast)
                t.mal-nil
                (t.make-list (t.get-value arg-ast)))
            ;;
            (t.string?* arg-ast)
            (let [a-str (t.get-value arg-ast)
                  str-len (length a-str)]
              (if (= str-len 0)
                  t.mal-nil
                  (do
                   (local str-tbl [])
                   (for [i 1 (length a-str)]
                     (table.insert str-tbl
                                   (t.make-string (string.sub a-str i i))))
                   (t.make-list str-tbl))))
            ;;
            (t.nil?* arg-ast)
            arg-ast
            ;;
            (u.throw*
             (t.make-string "Expected list, vector, string, or nil")))))))

(local mal-time-ms
  (t.make-fn
    (fn [asts]
        (t.make-number
         (math.floor (* 1000000 (os.clock)))))))

(fn fennel-eval*
  [fennel-val]
  (if (= "nil" (type fennel-val))
      t.mal-nil
      (= "boolean" (type fennel-val))
      (t.make-boolean fennel-val)
      (= "string" (type fennel-val))
      (t.make-string fennel-val)
      (= "number" (type fennel-val))
      (t.make-number fennel-val)
      (= "table" (type fennel-val))
      (t.make-list (u.map fennel-eval* fennel-val))
      (u.throw*
       (t.make-string (.. "Unsupported type: " (type fennel-val))))))

(local mal-fennel-eval
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "fennel-eval takes 1 argument")))
      (let [head-ast (. asts 1)]
        (when (not (t.string?* head-ast))
          (u.throw* (t.make-string
                     "fennel-eval first argument should be a string")))
        (let [(ok? result) (pcall fennel.eval (t.get-value head-ast))]
          (if ok?
              (fennel-eval* result)
              (u.throw*
               (t.make-string (.. "Eval failed: " result)))))))))

{"+" (t.make-fn (fn [asts]
                  (var total 0)
                  (each [i val (ipairs asts)]
                    (set total
                         (+ total (t.get-value val))))
                  (t.make-number total)))
 "-" (t.make-fn (fn [asts]
                  (var total 0)
                  (let [n-args (length asts)]
                    (if (= 0 n-args)
                        (t.make-number 0)
                        (= 1 n-args)
                        (t.make-number (- 0 (t.get-value (. asts 1))))
                        (do
                         (set total (t.get-value (. asts 1)))
                         (for [idx 2 n-args]
                           (let [cur (t.get-value (. asts idx))]
                             (set total
                                  (- total cur))))
                         (t.make-number total))))))
 "*" (t.make-fn (fn [asts]
                  (var total 1)
                  (each [i val (ipairs asts)]
                    (set total
                         (* total (t.get-value val))))
                  (t.make-number total)))
 "/" (t.make-fn (fn [asts]
                  (var total 1)
                  (let [n-args (length asts)]
                    (if (= 0 n-args)
                        (t.make-number 1)
                        (= 1 n-args)
                        (t.make-number (/ 1 (t.get-value (. asts 1))))
                        (do
                         (set total (t.get-value (. asts 1)))
                         (for [idx 2 n-args]
                           (let [cur (t.get-value (. asts idx))]
                             (set total
                                  (/ total cur))))
                          (t.make-number total))))))
 "list" mal-list
 "list?" mal-list?
 "empty?" mal-empty?
 "count" mal-count
 "=" mal-=
 "<" (t.make-fn (fn [asts]
                  (let [val-1 (t.get-value (. asts 1))
                        val-2 (t.get-value (. asts 2))]
                    (t.make-boolean (< val-1 val-2)))))
 "<=" (t.make-fn (fn [asts]
                   (let [val-1 (t.get-value (. asts 1))
                         val-2 (t.get-value (. asts 2))]
                     (t.make-boolean (<= val-1 val-2)))))
 ">" (t.make-fn (fn [asts]
                  (let [val-1 (t.get-value (. asts 1))
                        val-2 (t.get-value (. asts 2))]
                    (t.make-boolean (> val-1 val-2)))))
 ">=" (t.make-fn (fn [asts]
                   (let [val-1 (t.get-value (. asts 1))
                         val-2 (t.get-value (. asts 2))]
                     (t.make-boolean (>= val-1 val-2)))))
 "pr-str" mal-pr-str
 "str" mal-str
 "prn" mal-prn
 "println" mal-println
 "read-string" mal-read-string
 "slurp" mal-slurp
 "atom" mal-atom
 "atom?" mal-atom?
 "deref" mal-deref
 "reset!" mal-reset!
 "swap!" mal-swap!
 "cons" mal-cons
 "concat" mal-concat
 "vec" mal-vec
 "nth" mal-nth
 "first" mal-first
 "rest" mal-rest
 "throw" mal-throw
 "apply" mal-apply
 "map" mal-map
 "nil?" mal-nil?
 "true?" mal-true?
 "false?" mal-false?
 "symbol?" mal-symbol?
 "symbol" mal-symbol
 "keyword" mal-keyword
 "keyword?" mal-keyword?
 "vector" mal-vector
 "vector?" mal-vector?
 "sequential?" mal-sequential?
 "map?" mal-map?
 "hash-map" mal-hash-map
 "assoc" mal-assoc
 "dissoc" mal-dissoc
 "get" mal-get
 "contains?" mal-contains?
 "keys" mal-keys
 "vals" mal-vals
 "readline" mal-readline
 "meta" mal-meta
 "with-meta" mal-with-meta
 "string?" mal-string?
 "number?" mal-number?
 "fn?" mal-fn?
 "macro?" mal-macro?
 "conj" mal-conj
 "seq" mal-seq
 "time-ms" mal-time-ms
 "fennel-eval" mal-fennel-eval
}
