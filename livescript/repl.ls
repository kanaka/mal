require! {
    readline
}

{stdin, stdout} = process

USER_IS_HUMAN = !!(process.env.HUMAN or process.env.npm_package_config_human)

export run-repl = (rep, env = {}) ->

    rl = readline.create-interface {
        terminal: USER_IS_HUMAN
        input: stdin
        output: stdout
        completer: (current-names-completer env)
    }

    rl.set-prompt 'user> '

    rl.on \line, (mal) ->
        if rl._fragment
            mal = "#{ rl._fragment } #{ mal }"
            rl._fragment = null

        if /\S/.test mal
            try
                ret = rep mal
                console.log(ret) if ret?
            catch e
                if USER_IS_HUMAN and (e.name is \IncompleteSequenceError)
                    rl._fragment = mal + '\n'
                else
                    console.error e.stack ? e

        stdin.write if rl._fragment then '   >> ' else 'user> '

    rl.on \close, ->
        console.log '\nGoodbye!'
        process.exit!

    rl.prompt()

current-names-completer = (env, partial-line) -->
    matches = partial-line?.match /\w+$/
    return [[], partial-line] unless matches
    [current-symbol] = matches
    hits = [k for k of env when 0 is k.indexOf current-symbol]
    [hits, current-symbol]
