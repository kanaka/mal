(defn escape
  [a-str]
  (->> (buffer a-str)
       (peg/replace-all "\\" "\\\\")
       (peg/replace-all "\"" "\\\"")
       (peg/replace-all "\n" "\\n")
       string))

(defn code*
  [ast buf print_readably]
  (case (ast :tag)
    :boolean
    (buffer/push-string buf (ast :content))
    :nil
    (buffer/push-string buf (ast :content))
    :keyword
    (buffer/push-string buf (ast :content))
    :number
    (buffer/push-string buf (string (ast :content)))
    :string
    (if print_readably
      (buffer/push-string buf (string "\""
                                      (escape (ast :content))
                                      "\""))
      (buffer/push-string buf (ast :content)))
    :symbol
    (buffer/push-string buf (ast :content))
    #
    :list
    (do
      (buffer/push-string buf "(")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf print_readably)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf ")"))
    :hash-map
    (do
      (buffer/push-string buf "{")
      (var remove false)
      (eachp [k v] (ast :content)
            (code* k buf print_readably)
            (buffer/push-string buf " ")
            (code* v buf print_readably)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf "}"))
    :vector
    (do
      (buffer/push-string buf "[")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf print_readably)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf "]"))
    #
    :function
    (buffer/push-string buf "#<function>")
    #
    :atom
    (do
      (buffer/push-string buf "(atom ")
      (code* (ast :content) buf print_readably)
      (buffer/push-string buf ")"))
    #
    :exception
    (do
      (buffer/push-string buf "Error: ")
      (code* (ast :content) buf print_readably))))

(comment

  (let [buf @""]
    (code* {:tag :number
            :content 1}
      buf))
  # => @"1"

  )

(defn pr_str
  [ast print_readably]
  (let [buf @""]
    (code* ast buf print_readably)
    buf))

(comment

  (pr_str {:tag :number
           :content 1}
    false)
  # => @"1"

  )
