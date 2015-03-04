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
      (def! load-file 
            (fn* [f]
                 (eval (read-string (str "(do " (slurp f) ")" )))))
    """)
    [filename, ...args] = process.argv.slice(2)
    environment['*ARGV*'] = new MalList args.map(string)
    if filename
        eval-mal environment, new MalList [(sym "load-file"), (string filename)]
    else
        run-repl rep environment
