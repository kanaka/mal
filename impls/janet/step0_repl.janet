(defn READ
  [code-str]
  code-str)

(defn EVAL
  [ast]
  ast)

(defn PRINT
  [ast]
  ast)

(defn rep
  [code-str]
  (PRINT (EVAL (READ code-str))))

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
    (if (< 0 (length buf))
      (prin (rep buf))
      (break))))
