(import ./reader :prefix "")
(import ./printer :prefix "")

(defn READ
  [code-str]
  (read_str code-str))

(defn EVAL
  [ast]
  ast)

(defn PRINT
  [value]
  (pr_str value true))

(defn rep
  [code-str]
  (let [ds (READ code-str)]
    (when ds
      (PRINT (EVAL ds)))))

# getline gives problems
(defn getstdin [prompt buf]
  (file/write stdout prompt)
  (file/flush stdout)
  (file/read stdin :line buf))

(defn main
  [& args]
  (var buf nil)
  (while true
    (set buf @"")
    (getstdin "user> " buf)
    (if (= 0 (length buf))
      (break)
      (try
        (print (rep buf))
        ([err]
         (print err))))))
