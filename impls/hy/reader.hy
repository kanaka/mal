(import [hy.models [HyInteger :as Int HyKeyword :as Keyword
                    HyString :as Str HySymbol :as Sym]]
        [re])

(defclass Blank [Exception])

(defclass Reader []
  (defn --init-- [self tokens &optional [position 0]]
    (setv self.tokens tokens self.position position))
  (defn next [self]
    (setv self.position (+ 1 self.position))
    (get self.tokens (- self.position 1)))
  (defn peek [self]
    (if (> (len self.tokens) self.position)
      (get self.tokens self.position)
      None)))

(def tok-re (.compile re "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:[\\\\].|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}()'\"`@,;]+)"))
(def int-re (.compile re "-?[0-9]+$"))
(def str-re (.compile re "^\"(?:[\\\\].|[^\\\\\"])*\"$"))
(def str-bad-re (.compile re "^\".*$"))

(defn tokenize [str]
  (list-comp
    t
    (t (.findall re tok-re str))
    (!= (get t 0) ";")))

(defn unescape [s]
  (-> s (.replace "\\\\"   "\u029e")
        (.replace "\\\""   "\"")
        (.replace "\\n"    "\n")
        (.replace "\u029e" "\\")))

(defn read-atom [rdr]
  (setv token (.next rdr))
  (if
    (.match re int-re token) (int token)
    (.match re str-re token) (Str (unescape (cut token 1 -1)))
    (.match re str-bad-re token) (raise (Exception "expected '\"', got EOF"))
    (= ":" (get token 0))    (Keyword token)
    (= "nil" token)          None
    (= "true" token)         True
    (= "false" token)        False
    True                     (Sym token)))

(defn read-seq [rdr &optional [start "("] [end ")"]]
  (setv ast   (list)
        token (.next rdr))
  (if (!= token start)
    (raise (Exception (+ "expected '" start "'")))
    (do 
      (setv token (.peek rdr))
      (while (!= token end)
        (if (not token) (raise (Exception (+ "expected '" end
                                             ", got EOF"))))
        (.append ast (read-form rdr))
        (setv token (.peek rdr)))
      (.next rdr)
      ast)))

(defn read-form [rdr]
  (setv token (.peek rdr))
  (if
    (= ";" (get token 0)) (.next rdr)

    (= "'" token)  (do (.next rdr)
                       (tuple [(Sym "quote") (read-form rdr)]))
    (= "`" token)  (do (.next rdr)
                       (tuple [(Sym "quasiquote") (read-form rdr)]))
    (= "~" token)  (do (.next rdr)
                       (tuple [(Sym "unquote") (read-form rdr)]))
    (= "~@" token) (do (.next rdr)
                       (tuple [(Sym "splice-unquote")
                                      (read-form rdr)]))
    (= "^" token)  (do (.next rdr)
                       (setv meta (read-form rdr))
                       (tuple [(Sym "with-meta") (read-form rdr) meta]))
    (= "@" token)  (do (.next rdr)
                       (tuple [(Sym "deref") (read-form rdr)]))

    (= ")" token)  (raise (Exception "unexpected ')'"))
    (= "(" token)  (tuple (read-seq rdr "(" ")"))

    (= "]" token)  (raise (Exception "unexpected ')'"))
    (= "[" token)  (read-seq rdr "[" "]")

    (= "}" token)  (raise (Exception "unexpected '}'"))
    (= "{" token)  (dict (partition (read-seq rdr "{" "}") 2))

    True           (read-atom rdr)))

(defn read-str [str]
  (setv tokens (tokenize str))
  (if (= 0 (len tokens)) (raise (Blank "blank line")))
  (read-form (Reader tokens)))
