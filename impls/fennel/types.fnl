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
  [elts md]
  (local md (if md md mal-nil))
  {:tag :list
   :content elts
   :md md})

(fn make-vector
  [elts md]
  (local md (if md md mal-nil))
  {:tag :vector
   :content elts
   :md md})

(fn make-hash-map
  [elts md]
  (local md (if md md mal-nil))
  {:tag :hash-map
   :content elts
   :md md})

(fn make-fn
  [a-fn ast params env is-macro md]
  (local is-macro (if is-macro is-macro false))
  (local md (if md md mal-nil))
  {:tag :fn
   :content a-fn
   :ast ast
   :params params
   :env env
   :is-macro is-macro
   :md md})

(fn make-atom
  [ast]
  {:tag :atom
   :content ast})

(local mal-true (make-boolean true))

(local mal-false (make-boolean false))

;;

(fn get-value
  [ast]
  (. ast :content))

(fn get-type
  [ast]
  (. ast :tag))

(fn get-md
  [ast]
  (. ast :md))

;;

(fn get-is-macro
  [ast]
  (. ast :is-macro))

(fn get-ast
  [ast]
  (. ast :ast))

(fn get-params
  [ast]
  (. ast :params))

(fn get-env
  [ast]
  (. ast :env))

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

(fn fn?*
  [ast]
  (= :fn (. ast :tag)))

(fn atom?*
  [ast]
  (= :atom (. ast :tag)))

(fn macro?*
  [ast]
  (and (fn?* ast)
       (get-is-macro ast)))

;;

(fn macrofy
  [fn-ast]
  (local macro-ast {})
  (each [k v (pairs fn-ast)]
    (tset macro-ast k v))
  (tset macro-ast
        :is-macro true)
  macro-ast)

(fn clone-with-meta
  [fn-ast meta-ast]
  (local new-fn-ast {})
  (each [k v (pairs fn-ast)]
    (tset new-fn-ast k v))
  (tset new-fn-ast
        :md meta-ast)
  new-fn-ast)

;;

(fn set-atom-value!
  [atom-ast value-ast]
  (tset atom-ast
        :content value-ast))

(fn deref*
  [ast]
  (if (not (atom?* ast))
      ;; XXX
      (error (.. "Expected atom, got: " (get-type ast)))
      (get-value ast)))

(fn reset!*
  [atom-ast val-ast]
  (set-atom-value! atom-ast val-ast)
  val-ast)

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
 :make-atom make-atom
 ;;
 :mal-nil mal-nil
 :mal-true mal-true
 :mal-false mal-false
 ;;
 :get-value get-value
 :get-md get-md
 :get-is-macro get-is-macro
 :get-ast get-ast
 :get-params get-params
 :get-env get-env
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
 :fn?* fn?*
 :atom?* atom?*
 :macro?* macro?*
 ;;
 :macrofy macrofy
 :clone-with-meta clone-with-meta
 ;;
 :set-atom-value! set-atom-value!
 :deref* deref*
 :reset!* reset!*
 ;;
 :empty?* empty?*
 :true?* true?*
 :false?* false?*
 :equals?* equals?*
}
