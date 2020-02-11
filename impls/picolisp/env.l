(class +Env)
# data outer
(dm T (Outer Binds Exprs)
   (=: data (new))
   (=: outer Outer)
   (for (Binds Binds Binds)
      (if (<> (car Binds) '&)
         (set> This (pop 'Binds) (pop 'Exprs))
         (pop 'Binds)
         (set> This (pop 'Binds) (MAL-list Exprs)) ) ) )

(de MAL-env (Outer Binds Exprs)
   (new '(+Env) Outer Binds Exprs) )

(dm set> (Key Value)
   (put (: data) Key Value) )

(dm find> (Key)
   (or (get (: data) Key)
      (and (: outer) (find> @ Key)) ) )

(dm get> (Key)
   (or (find> This Key)
      (throw 'err (MAL-error (MAL-string (pack "'" Key "' not found")))) ) )
