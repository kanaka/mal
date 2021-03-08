(ns mal.core
  (:refer-clojure :exclude [pr-str])
  (:require [clojure.string :refer [join]]
            [mal.readline :as readline]
            [mal.reader :as reader]
            [mal.printer :refer [pr-str atom?]]))

;; Errors/exceptions
(defn mal_throw [obj]
  (throw (ex-info "mal exception" {:data obj})))

;; String functions
#?(:cljs (defn slurp [f] (.readFileSync (js/require "fs") f "utf-8")))

;; Numeric functions
#?(:clj  (defn time-ms [] (System/currentTimeMillis))
   :cljs (defn time-ms [] (.getTime (js/Date.))))

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
   ['number? number?]
   ['fn? (fn [o] (if (and (fn? o) (not (:ismacro (meta o)))) true false))]
   ['macro? (fn [o] (if (and (fn? o) (:ismacro (meta o))) true false))]

   ['pr-str (fn [& xs] (join " " (map #(pr-str % true) xs)))]
   ['str (fn [& xs] (join "" (map #(pr-str % false) xs)))]
   ['prn (fn [& xs] (println (join " " (map #(pr-str % true) xs))))]
   ['println (fn [& xs] (println (join " " (map #(pr-str % false) xs))))]
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
   ['keys (fn [hm] (let [ks (keys hm)] (if (nil? ks) '() ks)))]
   ['vals (fn [hm] (let [vs (vals hm)] (if (nil? vs) '() vs)))]

   ['sequential? sequential?]
   ['vec vec]
   ['cons cons]
   ['concat #(apply list (apply concat %&))]
   ['nth nth]
   ['first first]
   ['rest rest]
   ['empty? empty?]
   ['count count]
   ['apply apply]
   ['map #(apply list (map %1 %2))]

   ['conj conj]
   ['seq (fn [obj] (seq (if (string? obj) (map str obj) obj)))]

   ['with-meta mal_with_meta]
   ['meta mal_meta]
   ['atom atom]
   ['atom? atom?]
   ['deref deref]
   ['reset! reset!]
   ['swap! swap!]])
