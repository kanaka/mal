(local t (require :types))

(fn escape
  [a-str]
  (pick-values 1
               (-> a-str
                   (string.gsub "\\" "\\\\")
                   (string.gsub "\"" "\\\"")
                   (string.gsub "\n" "\\n"))))

(fn code*
  [ast buf print_readably]
  (let [value (t.get-value ast)]
    (if (t.nil?* ast)
        (table.insert buf value)
        ;;
        (t.boolean?* ast)
        (table.insert buf (if value "true" "false"))
        ;;
        (t.number?* ast)
        (table.insert buf (tostring value))
        ;;
        (t.keyword?* ast)
        (table.insert buf value)
        ;;
        (t.symbol?* ast)
        (table.insert buf value)
        ;;
        (t.string?* ast)
        (if print_readably
            (do
             (table.insert buf "\"")
             (table.insert buf (escape value))
             (table.insert buf "\""))
            (table.insert buf value))
        ;;
        (t.list?* ast)
        (do
         (table.insert buf "(")
         (var remove false)
         (each [idx elt (ipairs value)]
               (code* elt buf print_readably)
               (table.insert buf " ")
               (set remove true))
         (when remove
           (table.remove buf))
         (table.insert buf ")"))
        ;;
        (t.vector?* ast)
        (do
         (table.insert buf "[")
         (var remove false)
         (each [idx elt (ipairs value)]
               (code* elt buf print_readably)
               (table.insert buf " ")
               (set remove true))
         (when remove
           (table.remove buf))
         (table.insert buf "]"))
        ;;
        (t.hash-map?* ast)
        (do
         (table.insert buf "{")
         (var remove false)
         (each [idx elt (ipairs value)]
               (code* elt buf print_readably)
               (table.insert buf " ")
               (set remove true))
         (when remove
           (table.remove buf))
         (table.insert buf "}"))
        ;;
        (t.atom?* ast)
        (do
         (table.insert buf "(atom ")
         (code* (t.get-value ast) buf print_readably)
         (table.insert buf ")")))
    buf))

(fn pr_str
  [ast print_readably]
  (let [buf []]
    (code* ast buf print_readably)
    (table.concat buf)))

(comment

 (pr_str (t.make-number 1) false)

 )

{:pr_str pr_str}
