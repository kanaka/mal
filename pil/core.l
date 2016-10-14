(de MAL-= (A B)
   (let (A* (MAL-type A)
         B* (MAL-type B))
      (cond
         ((and (= A* 'map) (= B* 'map))
          # TODO
          NIL)
         ((and (memq A* '(list vector)) (memq B* '(list vector)))
          (MAL-seq-= (MAL-value A) (MAL-value B)) )
         ((= A* B*)
          (= (MAL-value A) (MAL-value B)) )
         (T NIL) ) ) )

(de MAL-seq-= (As Bs)
   (when (= (length As) (length Bs))
      (catch 'result
         (while As
            (ifn (MAL-= (pop 'As) (pop 'Bs))
               (throw 'result NIL) ) )
         T) ) )

(de MAL-seq? (X)
   (memq (MAL-type X) '(list vector)) )

(def '*Ns
   '((+ . `(MAL-fn '((A B) (MAL-number (+ (MAL-value A) (MAL-value B))))))
     (- . `(MAL-fn '((A B) (MAL-number (- (MAL-value A) (MAL-value B))))))
     (* . `(MAL-fn '((A B) (MAL-number (* (MAL-value A) (MAL-value B))))))
     (/ . `(MAL-fn '((A B) (MAL-number (/ (MAL-value A) (MAL-value B))))))

     (< . `(MAL-fn '((A B) (if (< (MAL-value A) (MAL-value B)) *MAL-true *MAL-false))))
     (<= . `(MAL-fn '((A B) (if (<= (MAL-value A) (MAL-value B)) *MAL-true *MAL-false))))
     (> . `(MAL-fn '((A B) (if (> (MAL-value A) (MAL-value B)) *MAL-true *MAL-false))))
     (>= . `(MAL-fn '((A B) (if (>= (MAL-value A) (MAL-value B)) *MAL-true *MAL-false))))

     (= . `(MAL-fn '((A B) (if (MAL-= A B) *MAL-true *MAL-false))))

     (list . `(MAL-fn '(@ (MAL-list (rest)))))
     (list? . `(MAL-fn '((X) (if (= (MAL-type X) 'list) *MAL-true *MAL-false))))
     (empty? . `(MAL-fn '((X) (if (and (MAL-seq? X) (not (MAL-value X))) *MAL-true *MAL-false))))
     (count . `(MAL-fn '((X) (if (MAL-seq? X) (MAL-number (length (MAL-value X))) (MAL-number 0)))))

     (pr-str . `(MAL-fn '(@ (MAL-string (glue " " (mapcar '((X) (pr-str X T)) (rest)))))))
     (str . `(MAL-fn '(@ (MAL-string (pack (mapcar pr-str (rest)))))))
     (prn . `(MAL-fn '(@ (prinl (glue " " (mapcar '((X) (pr-str X T)) (rest)))) *MAL-nil)))
     (println . `(MAL-fn '(@ (prinl (glue " " (mapcar pr-str (rest)))) *MAL-nil))) ) )
