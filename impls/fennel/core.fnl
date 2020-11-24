(local t (require :types))
(local u (require :utils))
(local printer (require :printer))

(local mal-list
  (t.make-fn
    (fn [asts]
      (t.make-list asts))))

(local mal-list?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "list? takes 1 argument")))
      (t.make-boolean (t.list?* (. asts 1))))))

(local mal-empty?
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "empty? takes 1 argument")))
      (let [arg-ast (. asts 1)]
        (if (t.nil?* arg-ast)
            t.mal-true
            (t.make-boolean (t.empty?* arg-ast)))))))

(local mal-count
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 1)
        (u.throw* (t.make-string "count takes 1 argument")))
      (let [arg-ast (. asts 1)]
        (if (t.nil?* arg-ast)
            (t.make-number 0)
            (t.make-number (length (t.get-value arg-ast))))))))

(local mal-=
  (t.make-fn
    (fn [asts]
      (when (< (length asts) 2)
        (u.throw* (t.make-string "= takes 2 arguments")))
      (let [ast-1 (. asts 1)
            ast-2 (. asts 2)]
        (if (t.equals?* ast-1 ast-2)
            t.mal-true
            t.mal-false)))))

(local mal-pr-str
  (t.make-fn
    (fn [asts]
      (local buf [])
      (when (> (length asts) 0)
        (each [i ast (ipairs asts)]
          (table.insert buf (printer.pr_str ast true))
          (table.insert buf " "))
        ;; remove extra space at end
        (table.remove buf))
      (t.make-string (table.concat buf)))))

(local mal-str
  (t.make-fn
    (fn [asts]
      (local buf [])
      (when (> (length asts) 0)
        (each [i ast (ipairs asts)]
          (table.insert buf (printer.pr_str ast false))))
      (t.make-string (table.concat buf)))))

(local mal-prn
  (t.make-fn
    (fn [asts]
      (local buf [])
      (when (> (length asts) 0)
        (each [i ast (ipairs asts)]
          (table.insert buf (printer.pr_str ast true))
          (table.insert buf " "))
        ;; remove extra space at end
        (table.remove buf))
      (print (table.concat buf))
      t.mal-nil)))

(local mal-println
  (t.make-fn
    (fn [asts]
      (local buf [])
      (when (> (length asts) 0)
        (each [i ast (ipairs asts)]
          (table.insert buf (printer.pr_str ast false))
          (table.insert buf " "))
        ;; remove extra space at end
        (table.remove buf))
      (print (table.concat buf))
      t.mal-nil)))

{"+" (t.make-fn (fn [asts]
                  (var total 0)
                  (each [i val (ipairs asts)]
                    (set total
                         (+ total (t.get-value val))))
                  (t.make-number total)))
 "-" (t.make-fn (fn [asts]
                  (var total 0)
                  (let [n-args (length asts)]
                    (if (= 0 n-args)
                        (t.make-number 0)
                        (= 1 n-args)
                        (t.make-number (- 0 (t.get-value (. asts 1))))
                        (do
                         (set total (t.get-value (. asts 1)))
                         (for [idx 2 n-args]
                           (let [cur (t.get-value (. asts idx))]
                             (set total
                                  (- total cur))))
                         (t.make-number total))))))
 "*" (t.make-fn (fn [asts]
                  (var total 1)
                  (each [i val (ipairs asts)]
                    (set total
                         (* total (t.get-value val))))
                  (t.make-number total)))
 "/" (t.make-fn (fn [asts]
                  (var total 1)
                  (let [n-args (length asts)]
                    (if (= 0 n-args)
                        (t.make-number 1)
                        (= 1 n-args)
                        (t.make-number (/ 1 (t.get-value (. asts 1))))
                        (do
                         (set total (t.get-value (. asts 1)))
                         (for [idx 2 n-args]
                           (let [cur (t.get-value (. asts idx))]
                             (set total
                                  (/ total cur))))
                          (t.make-number total))))))
 "list" mal-list
 "list?" mal-list?
 "empty?" mal-empty?
 "count" mal-count
 "=" mal-=
 "<" (t.make-fn (fn [asts]
                  (let [val-1 (t.get-value (. asts 1))
                        val-2 (t.get-value (. asts 2))]
                    (t.make-boolean (< val-1 val-2)))))
 "<=" (t.make-fn (fn [asts]
                   (let [val-1 (t.get-value (. asts 1))
                         val-2 (t.get-value (. asts 2))]
                     (t.make-boolean (<= val-1 val-2)))))
 ">" (t.make-fn (fn [asts]
                  (let [val-1 (t.get-value (. asts 1))
                        val-2 (t.get-value (. asts 2))]
                    (t.make-boolean (> val-1 val-2)))))
 ">=" (t.make-fn (fn [asts]
                   (let [val-1 (t.get-value (. asts 1))
                         val-2 (t.get-value (. asts 2))]
                     (t.make-boolean (>= val-1 val-2)))))
 "pr-str" mal-pr-str
 "str" mal-str
 "prn" mal-prn
 "println" mal-println
}
