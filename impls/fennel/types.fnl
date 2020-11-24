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

(local mal-true (make-boolean true))

(local mal-false (make-boolean false))

;;

(fn get-value
  [ast]
  (. ast :content))

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
 ;;
 :mal-nil mal-nil
 :mal-true mal-true
 :mal-false mal-false
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
 :get-value get-value
}
