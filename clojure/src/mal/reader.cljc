(ns mal.reader
    (:refer-clojure :exclude [read-string])
    #?(:clj  (:require [clojure.tools.reader :as r]
                       [clojure.tools.reader.reader-types :as rt]))
    #?(:cljs (:require [cljs.tools.reader :as r]
                       [cljs.tools.reader.reader-types :as rt])))

;; change tools.reader syntax-quote to quasiquote
(defn- wrap [sym]
  (fn [rdr _] (list sym (#'r/read rdr true nil))))

(defn- wrap-with [sym]
  (fn [rdr arg _] (list sym (#'r/read rdr true nil) arg)))

;; Override some tools.reader reader macros so that we can do our own
;; metadata and quasiquote handling
(def new-rmacros
  (fn [f]
    (fn [ch]
      (case ch
        \` (wrap 'quasiquote)
        \~ (fn [rdr comma]
             (if-let [ch (rt/peek-char rdr)]
               (if (identical? \@ ch)
                 ((wrap 'splice-unquote) (doto rdr rt/read-char) \@)
                 ((wrap 'unquote) rdr \~))))
        \^ (fn [rdr comma]
             (let [m (#'r/read rdr)]
               ((wrap-with 'with-meta) rdr m \^)))
        \@ (wrap 'deref)
        (f ch)))))

#?(:clj (alter-var-root #'r/macros new-rmacros)
   :cljs (set! r/macros (new-rmacros r/macros)))

(defn read-string [s]
  (r/read-string s))
