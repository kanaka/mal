require! {
    LiveScript
    fs
    readline
    './repl.ls': {run-repl}
    './core.ls': core
    './env.ls': {create-env}
    './types.ls': {Builtin, MalList, string, sym}
    './logic.ls': {eval-mal}
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
    'prelude-ls': {id}
}

{stdin, stdout} = process

read-mal = read-str

print-mal = (ast) ->
    if ast? then pr-str ast else null

rep = (env, expr) --> expr |> read-mal |> (eval-mal env) |> print-mal

core-mal = fs.readFileSync __dirname + '/core.mal', 'utf8'

let environment = create-env core.ns
    environment['eval'] = new Builtin (eval-mal environment) . (.0)
    eval-mal environment, (read-str core-mal)
    eval-mal environment, (read-str """
      (do
        (def! load-file 
                (fn* [f]
                    (eval (read-string (str "(do " (slurp f) ")" )))))

        (defmacro or [& xs]
            (if (empty? xs)
                nil
                (if (= 1 (count xs))
                    (first xs)
                    `(let* [or_FIXME ~(first xs)]
                           (if or_FIXME or_FIXME (or ~@(rest xs)))))))

        (defmacro cond [& xs]
            (let [c (count xs)]
                (if (> c 0)
                    (let [[fst snd & rst] xs]
                        (list 'if
                            fst
                            snd
                            (if (= 1 (count rst))
                                (first rst)
                                (cons 'cond rst)))))))
    )
    """)
    [filename, ...args] = process.argv.slice(2)
    environment['*ARGV*'] = new MalList args.map(string)
    if filename
        eval-mal environment, new MalList [(sym "load-file"), (string filename)]
    else
        run-repl rep environment
