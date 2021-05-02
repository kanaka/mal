(import ./reader)
(import ./printer)
(import ./types :as t)

(defn READ
  [code-str]
  (reader/read_str code-str))

(defn EVAL
  [ast]
  ast)

(defn PRINT
  [value]
  (printer/pr_str value true))

(defn rep
  [code-str]
  (PRINT (EVAL (READ code-str))))

# getline gives problems
(defn getstdin [prompt buf]
  (file/write stdout prompt)
  (file/flush stdout)
  (file/read stdin :line buf))

(defn handle-error
  [err]
  (cond
    (t/nil?* err)
    (print)
    ##
    (string? err)
    (print err)
    ##
    (print (string "Error: " (PRINT err)))))

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
         (handle-error err))))))
