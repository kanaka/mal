(class +MAL)
# type value meta
(dm T (Type Value Meta)
   (=: type Type)
   (=: value Value)
   (=: meta Meta) )

(class +MALTrue +MAL)
(dm T ()
   (super 'true T NIL) )

(class +MALFalse +MAL)
(dm T ()
   (super 'false NIL NIL) )

(class +MALNil +MAL)
(dm T ()
   (super 'nil NIL NIL) )

(def '*MAL-true (new '(+MALTrue)))
(def '*MAL-false (new '(+MALFalse)))
(def '*MAL-nil (new '(+MALNil)))

(class +MALNumber +MAL)
(dm T (Number)
   (super 'number Number NIL) )

(class +MALString +MAL)
(dm T (String)
   (super 'string String NIL) )

(class +MALSymbol +MAL)
(dm T (String)
   (super 'symbol String NIL) )

(class +MALKeyword +MAL)
(dm T (String)
   (super 'keyword String NIL) )

(class +MALList +MAL)
(dm T (Values)
   (super 'list Values NIL) )

(class +MALVector +MAL)
(dm T (Values)
   (super 'vector Values NIL) )

(class +MALMap +MAL)
(dm T (Values)
   (super 'map Values NIL) )

(class +MALAtom +MAL)
(dm T (Value)
   (super 'atom Value NIL) )

(class +MALError +MAL)
(dm T (Value)
   (super 'error Value NIL) )
