(import ./types :prefix "")
(import ./printer)

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

(defn is-list?*
  [ast]
  (= (ast :tag) :list))

(def is-list?
  (make-function
    (fn [asts]
      (if (is-list?* (in asts 0))
        (make-boolean true)
        (make-boolean false)))))

(defn is-vector?*
  [ast]
  (= (ast :tag) :vector))

(def is-vector?
  (make-function
    (fn [asts]
      (if (is-vector?* (in asts 0))
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
        (if (and (not (is-list?* ast-1))
                 (not (is-vector?* ast-1)))
          (= val-1 val-2)
          (if (not= (length val-1) (length val-2))
            false
            (do
              (var found-unequal false)
              (each [v1 v2] (partition 2 (interleave val-1 val-2))
                    (when (not (equals?* v1 v2))
                      (set found-unequal true)
                      (break)))
              (not found-unequal))))))))

(def equals?
  (make-function
    (fn [asts]
      (let [ast-1 (in asts 0)
            ast-2 (in asts 1)]
        (if (equals?* ast-1 ast-2)
          (make-boolean true)
          (make-boolean false))))))

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

(def ns
  {(make-symbol "+") (arith-fn +)
   (make-symbol "-") (arith-fn -)
   (make-symbol "*") (arith-fn *)
   (make-symbol "/") (arith-fn /)
   (make-symbol "list") create-list
   (make-symbol "list?") is-list?
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
   (make-symbol "println") println})
