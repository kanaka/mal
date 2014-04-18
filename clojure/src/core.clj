(ns core
  (:require [readline]))

;; Errors/exceptions
(defn mal_throw [obj]
  (throw (ex-info "mal exception" {:data obj})))

;; Number functions

(defn time-ms []
  (System/currentTimeMillis))

;; Metadata functions
;; - store metadata at :meta key of the real metadata
(defn mal_with_meta [obj m]
  (let [new-meta (assoc (meta obj) :meta m)]
    (with-meta obj new-meta)))

(defn mal_meta [obj]
  (:meta (meta obj)))

;; Atom functions
(defn atom? [atm]
  (= (type atm) clojure.lang.Atom))

;; core_ns is core namespaces functions
(def core_ns
  [['= =]
   ['throw mal_throw]
   ['nil? nil?]
   ['true? true?]
   ['false? false?]
   ['symbol? symbol?]

   ['pr-str pr-str]
   ['str str]
   ['prn prn]
   ['println println]
   ['readline readline/readline]
   ['read-string reader/read-string]
   ['slurp slurp]
   ['< <]
   ['<= <=]
   ['> >]
   ['>= >=]
   ['+ +]
   ['- -]
   ['* *]
   ['/ /]
   ['time-ms time-ms]
  
   ['list list]
   ['list? seq?]
   ['vector vector]
   ['vector? vector?]
   ['hash-map hash-map]
   ['map? map?]
   ['assoc assoc]
   ['dissoc dissoc]
   ['get get]
   ['contains? contains?]
   ['keys keys]
   ['vals vals]
   
   ['sequential? sequential?]
   ['cons cons]
   ['concat concat]
   ['nth nth]
   ['first first]
   ['rest rest]
   ['empty? empty?]
   ['count count]
   ['conj conj]
   ['apply apply]
   ['map #(doall (map %1 %2))] 

   ['with-meta mal_with_meta]
   ['meta mal_meta]
   ['atom atom]
   ['atom? atom?]
   ['deref deref]
   ['reset! reset!]
   ['swap! swap!]])
