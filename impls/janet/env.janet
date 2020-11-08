(import ./types :prefix "")

# based on janet's take-until
(defn split-before-and-where
  "Split ind before first position matching pred, and report on where.
   If there was no split, report index as nil."
  [pred ind]
  (def use-str (bytes? ind))
  (def f (if use-str string/slice tuple/slice))
  (def len (length ind))
  (def i (find-index pred ind))
  (def end (if (nil? i) len i))
  [(f ind 0 end) (f ind end) i])

# XXX: probably could be simplified
(defn make-env
  [&opt outer binds exprs]
  (default binds [])
  (default exprs [])
  (def [new-binds split-idx]
    (let [[normal-binds args i]
          (split-before-and-where (fn [sym-ast]
                                    (= (sym-ast :content) "&"))
                                   binds)]
      (if i
        [(array/concat (array ;normal-binds) (in args 1)) i]
        [normal-binds nil])))
  (def new-exprs
    (if split-idx
      (array/concat (array ;(slice exprs 0 split-idx))
                    (array (make-list (slice exprs split-idx))))
      exprs))
  @{:data (zipcoll new-binds new-exprs)
    :outer outer})

(defn env-set
  [env sym value]
  (put-in env [:data sym]
              value))

(defn env-find
  [env sym]
  (if (get-in env [:data sym])
    env
    (when-let [outer (get env :outer)]
      (env-find outer sym))))

(defn env-get
  [env sym]
  (if-let [goal-env (env-find env sym)]
    (get-in goal-env [:data sym])
    (error (string (sym :content) " not found." ))))
