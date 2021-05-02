(local t (require :types))
(local u (require :utils))

(local lpeg (require :lpeg))

(local P lpeg.P)

(local S lpeg.S)

(local C lpeg.C)

(local V lpeg.V)

(local Cmt lpeg.Cmt)

(fn unescape
  [a-str]
  (pick-values 1
               (-> a-str
                   (string.gsub "\\\\" "\u{029e}") ;; temporarily hide
                   (string.gsub "\\\"" "\"")
                   (string.gsub "\\n" "\n")
                   (string.gsub "\u{029e}" "\\")))) ;; now replace

(local grammar
  {1 "main"
  "main" (^ (V "input") 1)
  "input" (+ (V "gap") (V "form"))
  "gap" (+ (V "ws") (V "comment"))
  "ws" (^ (S " \f\n\r\t,") 1)
  "comment" (* ";"
               (^ (- (P 1) (S "\r\n"))
                  0))
  "form" (+ (V "boolean") (V "nil")
            (V "number") (V "keyword") (V "symbol") (V "string")
            (V "list") (V "vector") (V "hash-map")
            (V "deref") (V "quasiquote") (V "quote")
            (V "splice-unquote")
            (V "unquote")
            (V "with-meta"))
  "name-char" (- (P 1)
                 (S " \f\n\r\t,[]{}()'`~^@\";"))
  "nil" (Cmt (C (* (P "nil")
                   (- (V "name-char"))))
             (fn [s i a]
                 (values i t.mal-nil)))
  "boolean" (Cmt (C (* (+ (P "false") (P "true"))
                       (- (V "name-char"))))
                 (fn [s i a]
                     (values i (if (= a "true")
                                   t.mal-true
                                   t.mal-false))))
  "number" (Cmt (C (^ (- (P 1)
                         (S " \f\n\r\t,[]{}()'`~^@\";"))
                      1))
                (fn [s i a]
                    (let [result (tonumber a)]
                      (if result
                          (values i (t.make-number result))
                          nil))))
  "keyword" (Cmt (C (* ":"
                       (^ (V "name-char") 0)))
                 (fn [s i a]
                     (values i (t.make-keyword a))))
  "symbol" (Cmt (^ (V "name-char") 1)
                (fn [s i a]
                    (values i (t.make-symbol a))))
  "string" (* (P "\"")
              (Cmt (C (* (^ (- (P 1)
                               (S "\"\\"))
                            0)
                         (^ (* (P "\\")
                               (P 1)
                               (^ (- (P 1)
                                     (S "\"\\"))
                                  0))
                            0)))
                   (fn [s i a]
                       (values i (t.make-string (unescape a)))))
              (+ (P "\"")
                 (P (fn [s i]
                        (error "unbalanced \"")))))
  "list" (* (P "(")
            (Cmt (C (^ (V "input") 0))
                 (fn [s i a ...]
                     (values i (t.make-list [...]))))
            (+ (P ")")
               (P (fn [s i]
                      (error "unbalanced )")))))
  "vector" (* (P "[")
              (Cmt (C (^ (V "input") 0))
                   (fn [s i a ...]
                       (values i (t.make-vector [...]))))
              (+ (P "]")
                 (P (fn [s i]
                        (error "unbalanced ]")))))
  "hash-map" (* (P "{")
                (Cmt (C (^ (V "input") 0))
                     (fn [s i a ...]
                         (values i (t.make-hash-map [...]))))
                (+ (P "}")
                   (P (fn [s i]
                          (error "unbalanced }")))))
  "deref" (Cmt (C (* (P "@")
                     (V "form")))
               (fn [s i ...]
                   (let [content [(t.make-symbol "deref")]]
                     (table.insert content (. [...] 2))
                     (values i (t.make-list content)))))
  "quasiquote" (Cmt (C (* (P "`")
                          (V "form")))
                    (fn [s i ...]
                        (let [content [(t.make-symbol "quasiquote")]]
                          (table.insert content (. [...] 2))
                          (values i (t.make-list content)))))
  "quote" (Cmt (C (* (P "'")
                     (V "form")))
               (fn [s i ...]
                   (let [content [(t.make-symbol "quote")]]
                     (table.insert content (. [...] 2))
                     (values i (t.make-list content)))))
  "splice-unquote" (Cmt (C (* (P "~@")
                              (V "form")))
                        (fn [s i ...]
                            (let [content [(t.make-symbol "splice-unquote")]]
                              (table.insert content (. [...] 2))
                              (values i (t.make-list content)))))
  "unquote" (Cmt (C (* (P "~")
                       (V "form")))
                 (fn [s i ...]
                     (let [content [(t.make-symbol "unquote")]]
                       (table.insert content (. [...] 2))
                       (values i (t.make-list content)))))
  "with-meta" (Cmt (C (* (P "^")
                         (V "form")
                         (^ (V "gap") 1)
                         (V "form")))
                   (fn [s i ...]
                       (let [content [(t.make-symbol "with-meta")]]
                         (table.insert content (. [...] 3))
                         (table.insert content (. [...] 2))
                         (values i (t.make-list content)))))
  })

(comment

 (lpeg.match grammar "; hello")

 (lpeg.match grammar "nil")

 (lpeg.match grammar "true")

 (lpeg.match grammar "false")

 (lpeg.match grammar "1.2")

 (lpeg.match grammar "(+ 1 1)")

 (lpeg.match grammar "[:a :b :c]")

 (lpeg.match grammar "\"hello there\"")

 (lpeg.match grammar "\"hello\" there\"")

)

(fn read_str
  [a-str]
  (let [(ok? result) (pcall lpeg.match grammar a-str)]
    (if ok?
        (let [res-type (type result)]
          (if (= res-type "table")
              result
              (u.throw* t.mal-nil)))
        (u.throw*
         (t.make-string result)))))

(comment

 (read_str "; hello")

 (read_str "nil")

 (read_str "true")

 (read_str "false")

 (read_str "1.2")

 (read_str "(+ 1 1)")

 (read_str "[:a :b :c]")

 (read_str "\"hello there\"")

 (read_str "\"hello\" there\"")

 )

{:read_str read_str}
