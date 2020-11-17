(import ./types :as t)

(defn escape
  [a-str]
  (->> (buffer a-str)
       (peg/replace-all "\\" "\\\\")
       (peg/replace-all "\"" "\\\"")
       (peg/replace-all "\n" "\\n")
       string))

(defn code*
  [ast buf print_readably]
  (cond
    (or (t/boolean?* ast)
        (t/nil?* ast)
        (t/keyword?* ast)
        (t/symbol?* ast))
    (buffer/push-string buf (t/get-value ast))
    ##
    (t/number?* ast)
    (buffer/push-string buf (string (t/get-value ast)))
    ##
    (t/string?* ast)
    (if print_readably
      (buffer/push-string buf (string "\""
                                      (escape (t/get-value ast))
                                      "\""))
      (buffer/push-string buf (t/get-value ast)))
    ##
    (t/list?* ast)
    (do
      (buffer/push-string buf "(")
      (var remove false)
      (each elt (t/get-value ast)
            (code* elt buf print_readably)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf ")"))
    ##
    (t/hash-map?* ast)
    (do
      (buffer/push-string buf "{")
      (var remove false)
      (eachp [k v] (t/get-value ast)
            (code* k buf print_readably)
            (buffer/push-string buf " ")
            (code* v buf print_readably)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf "}"))
    ##
    (t/vector?* ast)
    (do
      (buffer/push-string buf "[")
      (var remove false)
      (each elt (t/get-value ast)
            (code* elt buf print_readably)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf "]"))
    ## XXX: what about macro?
    (t/fn?* ast)
    (buffer/push-string buf "#<function>")
    ##
    (t/atom?* ast)
    (do
      (buffer/push-string buf "(atom ")
      (code* (t/get-value ast) buf print_readably)
      (buffer/push-string buf ")"))
    ##
    (t/exception?* ast)
    (do
      (buffer/push-string buf "Error: ")
      (code* (t/get-value ast) buf print_readably))))

(comment

  (let [buf @""]
    (code* (make-number 1) buf false))
  # => @"1"

  )

(defn pr_str
  [ast print_readably]
  (let [buf @""]
    (code* ast buf print_readably)
    buf))

(comment

  (pr_str (make-number 1) false)
  # => @"1"

  )
