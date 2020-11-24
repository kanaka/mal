(fn make-nil
  [a-str]
  {:tag :nil
   :content "nil"})

(fn make-boolean
  [a-bool]
  {:tag :boolean
   :content a-bool})

(fn make-number
  [a-num]
  {:tag :number
   :content a-num})

(fn make-keyword
  [a-str]
  {:tag :keyword
   :content a-str})

(fn make-symbol
  [a-str]
  {:tag :symbol
   :content a-str})

(fn make-string
  [a-str]
  {:tag :string
   :content a-str})

(local mal-nil (make-nil))

(fn make-list
  [elts]
  {:tag :list
   :content elts})

(fn make-vector
  [elts]
  {:tag :vector
   :content elts})

(fn make-hash-map
  [elts]
  {:tag :hash-map
   :content elts})

(fn make-fn
  [a-fn]
  {:tag :fn
   :content a-fn})

(local mal-true (make-boolean true))

(local mal-false (make-boolean false))

;;

(fn get-value
  [ast]
  (. ast :content))

(fn get-type
  [ast]
  (. ast :tag))

;;

(fn nil?*
  [ast]
  (= :nil (. ast :tag)))

(fn boolean?*
  [ast]
  (= :boolean (. ast :tag)))

(fn number?*
  [ast]
  (= :number (. ast :tag)))

(fn keyword?*
  [ast]
  (= :keyword (. ast :tag)))

(fn symbol?*
  [ast]
  (= :symbol (. ast :tag)))

(fn string?*
  [ast]
  (= :string (. ast :tag)))

(fn list?*
  [ast]
  (= :list (. ast :tag)))

(fn vector?*
  [ast]
  (= :vector (. ast :tag)))

(fn hash-map?*
  [ast]
  (= :hash-map (. ast :tag)))

;;

(fn empty?*
  [ast]
  (when (or (list?* ast)
            (vector?* ast))
    (= (length (get-value ast)) 0)))

(fn true?*
  [ast]
  (and (boolean?* ast)
       (= true (get-value ast))))

(fn false?*
  [ast]
  (and (boolean?* ast)
       (= false (get-value ast))))

(fn equals?*
  [ast-1 ast-2]
  (let [type-1 (get-type ast-1)
        type-2 (get-type ast-2)]
    (if (and (not= type-1 type-2)
             ;; XXX: not elegant
             (not (and (list?* ast-1) (vector?* ast-2)))
             (not (and (list?* ast-2) (vector?* ast-1))))
      false
      (let [val-1 (get-value ast-1)
            val-2 (get-value ast-2)]
        ;; XXX: when not a collection...
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
                (var idx 1)
                (while (and (not found-unequal)
                            (<= idx (length val-1)))
                  (let [v1 (. val-1 idx)
                        v2 (. val-2 idx)]
                    (when (not (equals?* v1 v2))
                      (set found-unequal true))
                    (set idx (+ idx 1))))
                (not found-unequal))
              (if (or (not (hash-map?* ast-1))
                      (not (hash-map?* ast-2)))
                false
                (do
                 (var found-unequal false)
                 (var idx-in-1 1)
                 (while (and (not found-unequal)
                             (<= idx-in-1 (length val-1)))
                   (let [k1 (. val-1 idx-in-1)]
                     (var found-in-2 false)
                     (var idx-in-2 1)
                     (while (and (not found-in-2)
                                 (<= idx-in-2 (length val-2)))
                       (let [k2 (. val-2 idx-in-2)]
                         (if (equals?* k1 k2)
                             (set found-in-2 true)
                             (set idx-in-2 (+ idx-in-2 2)))))
                     (if (not found-in-2)
                         (set found-unequal true)
                         (let [v1 (. val-1 (+ idx-in-1 1))
                               v2 (. val-2 (+ idx-in-2 1))]
                           (if (not (equals?* v1 v2))
                               (set found-unequal true)
                               (set idx-in-1 (+ idx-in-1 2)))))))
                  (not found-unequal))))))))))

{
 :make-nil make-nil
 :make-boolean make-boolean
 :make-number make-number
 :make-keyword make-keyword
 :make-symbol make-symbol
 :make-string make-string
 :make-list make-list
 :make-vector make-vector
 :make-hash-map make-hash-map
 :make-fn make-fn
 ;;
 :mal-nil mal-nil
 :mal-true mal-true
 :mal-false mal-false
 ;;
 :get-value get-value
 ;;
 :nil?* nil?*
 :boolean?* boolean?*
 :number?* number?*
 :keyword?* keyword?*
 :symbol?* symbol?*
 :string?* string?*
 :list?* list?*
 :vector?* vector?*
 :hash-map?* hash-map?*
 ;;
 :empty?* empty?*
 :true?* true?*
 :false?* false?*
 :equals?* equals?*
}
