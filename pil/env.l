(class +Env)
# data outer
(dm T (Outer)
   (=: data (new))
   (=: outer Outer) )

(de MAL-env (Outer)
   (new '(+Env) Outer) )

(dm set> (Key Value)
   (put (: data) Key Value) )

(dm find> (Key)
   (or (get (: data) Key)
      (and (: outer) (find> @ Key)) ) )

(dm get> (Key)
   (or (find> This Key)
      (throw 'err (MAL-error (pack "'" Key "' not found"))) ) )
