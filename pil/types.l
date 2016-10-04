(class +MAL)
# type value meta
(dm T (Type Value Meta)
   (=: type Type)
   (=: value Value)
   (=: meta Meta) )

(de MAL-type (MAL)
   (get MAL 'type) )

(de MAL-value (MAL)
   (get MAL 'value) )

(de MAL-meta (MAL)
   (get MAL 'meta) )

(class +MALTrue +MAL)
(dm T ()
   (super 'true 'true NIL) )

(class +MALFalse +MAL)
(dm T ()
   (super 'false 'false NIL) )

(class +MALNil +MAL)
(dm T ()
   (super 'nil 'nil NIL) )

(def '*MAL-true (new '(+MALTrue)))
(def '*MAL-false (new '(+MALFalse)))
(def '*MAL-nil (new '(+MALNil)))

(class +MALNumber +MAL)
(dm T (Number)
   (super 'number Number NIL) )

(de MAL-number (N)
   (new '(+MALNumber) N) )

(class +MALString +MAL)
(dm T (String)
   (super 'string String NIL) )

(de MAL-string (N)
   (new '(+MALString) N) )

(class +MALSymbol +MAL)
(dm T (String)
   (super 'symbol String NIL) )

(de MAL-symbol (N)
   (new '(+MALSymbol) N) )

(class +MALKeyword +MAL)
(dm T (String)
   (super 'keyword String NIL) )

(de MAL-keyword (N)
   (new '(+MALKeyword) N) )

(class +MALList +MAL)
(dm T (Values)
   (super 'list Values NIL) )

(de MAL-list (N)
   (new '(+MALList) N) )

(class +MALVector +MAL)
(dm T (Values)
   (super 'vector Values NIL) )

(de MAL-vector (N)
   (new '(+MALVector) N) )

(class +MALMap +MAL)
(dm T (Values)
   (super 'map Values NIL) )

(de MAL-map (N)
   (new '(+MALMap) N) )

(class +MALAtom +MAL)
(dm T (Value)
   (super 'atom Value NIL) )

(de MAL-atom (N)
   (new '(+MALAtom) N) )

(class +MALFn +MAL)
(dm T (Fn)
   (super 'fn Fn NIL) )

(de MAL-fn (Fn)
   (new '(+MALFn) Fn) )

(class +MALError +MAL)
(dm T (Value)
   (super 'error Value NIL) )

(de MAL-error (Value)
   (new '(+MALError) Value) )
