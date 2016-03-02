(ns core
  (:require [readline]
            [printer]))

;; Errors/exceptions
(defn mal_throw [obj]
  (throw (ex-info "mal exception" {:data obj})))

;; Metadata functions
;; - store metadata at :meta key of the real metadata
(defn mal_with_meta [obj m]
  (let [new-meta (assoc (meta obj) :meta m)]
    (with-meta obj new-meta)))

(defn mal_meta [obj]
  (:meta (meta obj)))

;; core_ns is core namespaces functions
(def core_ns
  [['= =]
   ['throw mal_throw]
   ['nil? nil?]
   ['true? true?]
   ['false? false?]
   ['string? string?]
   ['symbol symbol]
   ['symbol? symbol?]
   ['keyword keyword]
   ['keyword? keyword?]

   ['pr-str pr-str]
   ['str printer/_str]
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
   ['time-ms (fn time-ms [] (System/currentTimeMillis))]
  
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
   ['keys (fn [hm] (let [ks (keys hm)] (if (nil? ks) '() ks)))]
   ['vals (fn [hm] (let [vs (vals hm)] (if (nil? vs) '() vs)))]
   
   ['sequential? sequential?]
   ['cons cons]
   ['concat concat]
   ['nth nth]
   ['first first]
   ['rest rest]
   ['empty? empty?]
   ['count count]
   ['apply apply]
   ['map #(doall (map %1 %2))] 

   ['conj conj]
   ['seq (fn [obj] (seq (if (string? obj) (map str obj) obj)))]

   ['with-meta mal_with_meta]
   ['meta mal_meta]
   ['atom atom]
   ['atom? (fn atom? [atm] (= (type atm) clojure.lang.Atom))]
   ['deref deref]
   ['reset! reset!]
   ['swap! swap!]])
