(ns types)

;; Custom printing

(defmethod clojure.core/print-method clojure.lang.Atom [a writer]
  (.write writer "(atom ")
  (.write writer (pr-str @a))
  (.write writer ")"))

;; Errors/exceptions
(defn mal_throw [obj]
  (throw (ex-info "mal exception" {:data obj})))


;; Atoms
(defn atom? [atm]
  (= (type atm) clojure.lang.Atom))


;; env 

(defn env [& [outer binds exprs]]
  ;;(prn "env" binds exprs)
  ;; (when (not= (count binds) (count exprs))
  ;;  (throw (Exception. "Arity mistmatch in env call")))
  (atom
    (loop [env {:outer outer}
           b binds
           e exprs]
      (cond
        (= nil b)
        env
        
        (= '& (first b))
        (assoc env (nth b 1) e)
  
        :else
        (recur (assoc env (first b) (first e)) (next b) (next e))))))

(defn env-find [env k]
  (cond
    (contains? @env k) env
    (:outer @env) (env-find (:outer @env) k)
    :else nil))

(defn env-get [env k]
  (let [e (env-find env k)]
    (when-not e
      (throw (Exception. (str "'" k "' not found"))))
    (get @e k)))

(defn env-set [env k v]
  (swap! env assoc k v)
  v)

(def types_ns
  [['pr-str pr-str] ['str str] ['prn prn] ['println println]
   ['with-meta with-meta] ['meta meta] ['= =]
   ['nil? nil?] ['true? true?] ['false? false?] ['symbol? symbol?]
   ['> >] ['>= >=] ['< <] ['<= <=] ['+ +] ['- -] ['* *] ['/ /]
   ['hash-map hash-map] ['map? map?]
   ['assoc assoc] ['dissoc dissoc] ['get get]
   ['contains? contains?] ['keys keys] ['vals vals]
   ['throw mal_throw]
   ['list list] ['list? seq?] ['vector vector] ['vector? vector?]
   ['atom atom] ['atom? atom?] ['deref deref]
   ['reset! reset!] ['swap! swap!]
   ['sequential? sequential?] ['cons cons] ['nth nth]
   ['empty? empty?] ['count count] ['concat concat]
   ['conj conj] ['first first] ['rest rest]
   ['apply apply] ['map #(doall (map %1 %2))]])
