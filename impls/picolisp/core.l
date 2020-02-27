(de MAL-= (A B)
   (let (A* (MAL-type A)
         B* (MAL-type B))
      (cond
         ((and (= A* 'map) (= B* 'map))
          (MAL-map-= (MAL-value A) (MAL-value B)) )
         ((and (memq A* '(list vector)) (memq B* '(list vector)))
          (MAL-seq-= (MAL-value A) (MAL-value B)) )
         ((= A* B*)
          (= (MAL-value A) (MAL-value B)) )
         (T NIL) ) ) )

(de MAL-map-= (As Bs)
   (when (= (length As) (length Bs))
      (let (As* (chunk As) Bs* (chunk Bs))
         (catch 'result
            (while As*
               (let (A (pop 'As*) Key (MAL-value (car A)) Val (cdr A)
                     B (find '((X) (= Key (MAL-value (car X)))) Bs*) )
                  (when (or (not B) (not (MAL-= Val (cdr B))))
                     (throw 'result NIL) ) ) )
            T ) ) ) )

(de MAL-seq-= (As Bs)
   (when (= (length As) (length Bs))
      (catch 'result
         (while As
            (ifn (MAL-= (pop 'As) (pop 'Bs))
               (throw 'result NIL) ) )
         T ) ) )

(de MAL-seq? (X)
   (memq (MAL-type X) '(list vector)) )

(de MAL-f (X)
   (MAL-value (if (isa '+Func X) (get X 'fn) X)) )

(de MAL-swap! @
   (let (X (next) Fn (next) Args (rest))
      (put X 'value (apply (MAL-f Fn) Args (MAL-value X))) ) )

(de MAL-nth (Seq N)
   (let (Seq* (MAL-value Seq) N* (MAL-value N))
      (if (< N* (length Seq*))
         (nth Seq* (inc N*) 1)
         (throw 'err (MAL-error (MAL-string "out of bounds"))) ) ) )

(de chunk (List)
   (make
      (for (L List L (cddr L))
         (link (cons (car L) (cadr L))) ) ) )

(de join (List)
   (mapcan '((X) (list (car X) (cdr X))) List) )

(de MAL-assoc @
   (let (Map (next) Args (rest))
      (MAL-map
         (append Args
            (join
               (filter '((X) (not (find '((Y) (MAL-= (car Y) (car X)))
                                     (chunk Args) ) ) )
                  (chunk (MAL-value Map)) ) ) ) ) ) )

(de MAL-dissoc @
   (let (Map (next) Args (rest))
      (MAL-map
         (make
            (for (L (MAL-value Map) L (cddr L))
               (unless (find '((X) (MAL-= (car L) X)) Args)
                  (link (car L) (cadr L)) ) ) ) ) ) )

(de MAL-seq (X)
   (if (or (= (MAL-type X) 'nil) (not (MAL-value X)))
      *MAL-nil
      (case (MAL-type X)
         (list X)
         (vector (MAL-list (MAL-value X)))
         (string (MAL-list (mapcar MAL-string (chop (MAL-value X))))) ) ) )

(de MAL-conj @
   (let (Seq (next) Args (rest))
      (if (= (MAL-type Seq) 'vector)
         (MAL-vector (append (MAL-value Seq) Args))
         (MAL-list (append (reverse Args) (MAL-value Seq))) ) ) )

(de clone (X)
   (let X* (new (val X))
      (maps '((C) (put X* (cdr C) (car C))) X)
      X* ) )

(de pil-to-mal (X)
   (cond
      ((not X) *MAL-nil)
      ((=T X) *MAL-true)
      ((num? X) (MAL-number X))
      ((str? X) (MAL-string X))
      ((sym? X) (MAL-symbol X))
      ((lst? X) (MAL-list (mapcar pil-to-mal X)))
      (T (MAL-string (sym X))) ) )

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
     (println . `(MAL-fn '(@ (prinl (glue " " (mapcar pr-str (rest)))) *MAL-nil)))

     (read-string . `(MAL-fn '((X) (read-str (MAL-value X)))))
     (slurp . `(MAL-fn '((X) (MAL-string (in (MAL-value X) (till NIL T))))))

     (atom . `(MAL-fn '((X) (MAL-atom X))))
     (atom? . `(MAL-fn '((X) (if (= (MAL-type X) 'atom) *MAL-true *MAL-false))))
     (deref . `(MAL-fn '((X) (MAL-value X))))
     (reset! . `(MAL-fn '((X Value) (put X 'value Value))))
     (swap! . `(MAL-fn MAL-swap!))

     (cons . `(MAL-fn '((X Seq) (MAL-list (cons X (MAL-value Seq))))))
     (concat . `(MAL-fn '(@ (MAL-list (apply append (mapcar MAL-value (rest)))))))

     (nth . `(MAL-fn MAL-nth))
     (first . `(MAL-fn '((X) (if (MAL-seq? X) (or (car (MAL-value X)) *MAL-nil) *MAL-nil))))
     (rest . `(MAL-fn '((X) (if (MAL-seq? X) (MAL-list (cdr (MAL-value X))) (MAL-list NIL)))))

     (throw . `(MAL-fn '((X) (throw 'err (MAL-error X)))))

     (apply . `(MAL-fn '(@ (let (Fn (next) X (rest)) (apply (MAL-f Fn) (append (head -1 X) (MAL-value (last X))))))))
     (map . `(MAL-fn '((Fn Seq) (MAL-list (mapcar (MAL-f Fn) (MAL-value Seq))))))

     (nil? . `(MAL-fn '((X) (if (= (MAL-type X) 'nil) *MAL-true *MAL-false))))
     (true? . `(MAL-fn '((X) (if (= (MAL-type X) 'true) *MAL-true *MAL-false))))
     (false? . `(MAL-fn '((X) (if (= (MAL-type X) 'false) *MAL-true *MAL-false))))
     (number? . `(MAL-fn '((X) (if (= (MAL-type X) 'number) *MAL-true *MAL-false))))
     (symbol? . `(MAL-fn '((X) (if (= (MAL-type X) 'symbol) *MAL-true *MAL-false))))
     (keyword? . `(MAL-fn '((X) (if (= (MAL-type X) 'keyword) *MAL-true *MAL-false))))
     (string? . `(MAL-fn '((X) (if (= (MAL-type X) 'string) *MAL-true *MAL-false))))
     (vector? . `(MAL-fn '((X) (if (= (MAL-type X) 'vector) *MAL-true *MAL-false))))
     (map? . `(MAL-fn '((X) (if (= (MAL-type X) 'map) *MAL-true *MAL-false))))
     (sequential? . `(MAL-fn '((X) (if (MAL-seq? X) *MAL-true *MAL-false))))
     (fn? . `(MAL-fn '((X) (if (or (= (MAL-type X) 'fn) (and (= (MAL-type X) 'func) (not (get X 'is-macro)))) *MAL-true *MAL-false))))
     (macro? . `(MAL-fn '((X) (if (and (= (MAL-type X) 'func) (get X 'is-macro)) *MAL-true *MAL-false))))

     (symbol . `(MAL-fn '((Name) (MAL-symbol (MAL-value Name)))))
     (keyword . `(MAL-fn '((X) (if (= (MAL-type X) 'keyword) X (MAL-keyword (MAL-value X))))))
     (vector . `(MAL-fn '(@ (MAL-vector (rest)))))
     (hash-map . `(MAL-fn '(@ (MAL-map (rest)))))

     (assoc . `(MAL-fn MAL-assoc))
     (dissoc . `(MAL-fn MAL-dissoc))
     (get . `(MAL-fn '((Map Key) (or (and (<> (MAL-type Map) 'nil) (cdr (find '((X) (MAL-= (car X) Key)) (chunk (MAL-value Map))))) *MAL-nil))))
     (contains? . `(MAL-fn '((Map Key) (if (find '((X) (MAL-= (car X) Key)) (chunk (MAL-value Map))) *MAL-true *MAL-false))))
     (keys . `(MAL-fn '((Map) (MAL-list (mapcar car (chunk (MAL-value Map)))))))
     (vals . `(MAL-fn '((Map) (MAL-list (extract cdr (chunk (MAL-value Map)))))))

     (with-meta . `(MAL-fn '((X Meta) (let X* (clone X) (put X* 'meta Meta) X*))))
     (meta . `(MAL-fn '((X) (or (MAL-meta X) *MAL-nil))))

     (readline . `(MAL-fn '((Prompt) (let Output (readline (MAL-value Prompt)) (if (=0 Output) NIL (MAL-string Output))))))
     (time-ms . `(MAL-fn '(() (MAL-number (/ (usec) 1000)))))
     (conj . `(MAL-fn MAL-conj))
     (seq . `(MAL-fn MAL-seq))

     (pil-eval . `(MAL-fn '((Input) (pil-to-mal (run (str (MAL-value Input))))))) ) )
