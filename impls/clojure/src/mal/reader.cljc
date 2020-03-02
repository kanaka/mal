(ns mal.reader
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as S]))

(defn throw-str [s]
  (throw #?(:cljs (js/Error. s)
           :clj (Exception. s))))

(defn rdr [tokens]
  {:tokens (vec tokens) :position (atom 0)})

(defn rdr-peek [rdr]
  (get (vec (:tokens rdr)) @(:position rdr)))

(defn rdr-next [rdr]
  (get (vec (:tokens rdr)) (dec (swap! (:position rdr) inc))))

(def tok-re #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:[\\].|[^\\\"])*\"?|;.*|[^\s\[\]{}()'\"`@,;]+)")
(def int-re #"^-?[0-9]+$")
(def str-re #"^\"((?:[\\].|[^\\\"])*)\"$")
(def badstr-re #"^\"")

(defn tokenize [s]
  (filter #(not= \; (first %))
          (map second (re-seq tok-re s))))

(defn unescape [s]
  (-> s (S/replace "\\\\" "\u029e")
        (S/replace "\\\"" "\"")
        (S/replace "\\n" "\n")
        (S/replace "\u029e" "\\")))

(defn read-atom [rdr]
  (let [token (rdr-next rdr)]
    (cond
     (re-seq int-re token)    #?(:cljs (js/parseInt token)
                                 :clj (Integer/parseInt token))
     (re-seq str-re token)    (unescape (second (re-find str-re token)))
     (re-seq badstr-re token) (throw-str (str "expected '\"', got EOF"))
     (= \: (get token 0))     (keyword (subs token 1))
     (= "nil" token)          nil
     (= "true" token)         true
     (= "false" token)        false
     :else                    (symbol token))))

(declare read-form)

(defn read-seq [rdr start end]
  (assert (= start (rdr-next rdr))) ;; pull off start
  (loop [lst []]
    (let [token (rdr-peek rdr)]
      (cond
        (= token end) (do (rdr-next rdr) lst)
        (not token) (throw-str (str "expected '" end "', got EOF"))
        :else (recur (conj lst (read-form rdr)))))))

(defn read-form [rdr]
  (let [tok (rdr-peek rdr)]
    (cond
      (= "'" tok)  (do (rdr-next rdr) (list 'quote (read-form rdr)))
      (= "`" tok)  (do (rdr-next rdr) (list 'quasiquote (read-form rdr)))
      (= "~" tok)  (do (rdr-next rdr) (list 'unquote (read-form rdr)))
      (= "~@" tok) (do (rdr-next rdr) (list 'splice-unquote (read-form rdr)))

      (= "^" tok)  (do (rdr-next rdr) (let [m (read-form rdr)]
                                          (list 'with-meta (read-form rdr) m)))
      (= "@" tok)  (do (rdr-next rdr) (list 'deref (read-form rdr)))

      (= ")" tok)  (throw-str "unexpected ')'")
      (= "(" tok)  (apply list (read-seq rdr "(" ")"))

      (= "]" tok)  (throw-str "unexpected ']'")
      (= "[" tok)  (vec (read-seq rdr "[" "]"))

      (= "}" tok)  (throw-str "unexpected '}'")
      (= "{" tok)  (apply hash-map (read-seq rdr "{" "}"))

      :else        (read-atom rdr))))

(defn read-string [s]
  (read-form (rdr (tokenize s))))
