(local printer (require :printer))
(local reader (require :reader))
(local t (require :types))
(local u (require :utils))

(local repl_env
  {"+" (fn [ast-1 ast-2]
         (t.make-number (+ (t.get-value ast-1)
                           (t.get-value ast-2))))
   "-" (fn [ast-1 ast-2]
         (t.make-number (- (t.get-value ast-1)
                          (t.get-value ast-2))))
   "*" (fn [ast-1 ast-2]
         (t.make-number (* (t.get-value ast-1)
                           (t.get-value ast-2))))
   "/" (fn [ast-1 ast-2]
         (t.make-number (/ (t.get-value ast-1)
                           (t.get-value ast-2))))})

(fn READ
  [code-str]
  (reader.read_str code-str))

;; forward declaration
(var EVAL 1)

(fn eval_ast
  [ast env]
  (if (t.symbol?* ast)
      (. env (t.get-value ast))
      ;;
      (t.list?* ast)
      (t.make-list (u.map (fn [elt-ast]
                            (EVAL elt-ast env))
                          (t.get-value ast)))
      ;;
      (t.vector?* ast)
      (t.make-vector (u.map (fn [elt-ast]
                              (EVAL elt-ast env))
                            (t.get-value ast)))
      ;;
      (t.hash-map?* ast)
      (t.make-hash-map (u.map (fn [elt-ast]
                                (EVAL elt-ast env))
                              (t.get-value ast)))
      ;;
      ast))

(set EVAL
  (fn [ast env]
      (if (not (t.list?* ast))
          (eval_ast ast env)
          ;;
          (t.empty?* ast)
          ast
          ;;
          (let [eval-list (eval_ast ast env)
                f (u.first (t.get-value eval-list))
                args (u.slice (t.get-value eval-list) 2 -1)]
            (f (table.unpack args))))))

(fn PRINT
  [ast]
  (printer.pr_str ast true))

(fn rep
  [code-str]
  (PRINT (EVAL (READ code-str) repl_env)))

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

