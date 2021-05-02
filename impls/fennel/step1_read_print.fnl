(local printer (require :printer))
(local reader (require :reader))
(local t (require :types))

(fn READ
  [code-str]
  (reader.read_str code-str))

(fn EVAL
  [ast]
  ast)

(fn PRINT
  [ast]
  (printer.pr_str ast true))

(fn rep
  [code-str]
  (PRINT (EVAL (READ code-str))))

(fn handle-error
  [err]
  (if (t.nil?* err)
      (print)
      (= "string" (type err))
      (print err)
      (print (.. "Error: " (PRINT err)))))

(var done false)

(while (not done)
  (io.write "user> ")
  (io.flush)
  (let [input (io.read)]
    (if (not input)
        (set done true)
        (xpcall (fn []
                  (print (rep input)))
                handle-error))))
