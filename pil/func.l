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
