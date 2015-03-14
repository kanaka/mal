# MAL implementation in LiveScript

require! {
    LiveScript
    fs
    './repl.ls': {run-repl}
    './core.ls': core
    './env.ls': {create-env}
    './types.ls': {Builtin, MalList, string, sym}
    './logic.ls': {eval-mal}
    './reader.ls': {read-str}
    './printer.ls': {pr-str}
}

read-mal = read-str

print-mal = (ast) -> if ast? then pr-str ast else null

rep = (env, expr) --> expr |> read-mal |> (eval-mal env) |> print-mal

core-mal = fs.readFileSync __dirname + '/core.mal', 'utf8'

let env = create-env core.ns
    evaluate = eval-mal env
    env['eval'] = new Builtin evaluate . (.0)
    evaluate (read-str core-mal)
    [filename, ...args] = process.argv.slice(2)
    env['*ARGV*'] = new MalList args.map(string)
    if filename
        evaluate new MalList [(sym "load-file"), (string filename)]
    else
        run-repl (rep env), env
