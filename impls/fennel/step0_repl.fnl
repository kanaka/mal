(fn READ [code-str]
    code-str)

(fn EVAL [ast]
    ast)

(fn PRINT [ast]
    ast)

(fn rep [code-str]
    (PRINT (EVAL (READ code-str))))

(var done false)

(while (not done)
  (io.write "user> ")
  (io.flush)
  (let [input (io.read)]
    (if (not input)
        (set done true)
        (print (rep input)))))
