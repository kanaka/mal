(class +Func)
# env ast params fn
(dm T (Env Ast Params Fn)
   (=: type 'func) # HACK
   (=: env Env)
   (=: ast Ast)
   (=: params Params)
   (=: fn Fn) )

(de MAL-func (Env Ast Params Fn)
   (new '(+Func) Env Ast Params Fn) )

(de MAL-macro (MalFn)
   (let (env    (get MalFn 'env)
         ast    (get MalFn 'ast)
         params (get MalFn 'params)
         fn     (get MalFn 'fn)
         clone  (MAL-func env ast params fn))
      (put clone 'is-macro T)
      clone))
