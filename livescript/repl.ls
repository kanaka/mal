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

    rl.setPrompt 'user> '

    rl.on \line, (mal) ->
        if rl._fragment
            mal = rl._fragment + mal
            rl._fragment = null

        if mal is \exit
            return rl.close()

        if /\S/.test mal
            try
                ret = rep mal
                if ret?.then
                    ret.then (handle-response rl), (handle-error rl, mal)
                else
                    handle-response rl, ret
            catch e
                handle-error rl, mal, e
        else
            re-prompt rl

    rl.on \close, ->
        console.log '\nGoodbye!'
        process.exit!

    re-prompt rl

re-prompt = (rl) ->
    stdin.write if rl._fragment then '  ..> ' else 'user> '

handle-response = (rl, ret) -->
    console.log(ret) if ret?
    re-prompt rl

handle-error = (rl, mal, e) -->
    if USER_IS_HUMAN and (e.name is \IncompleteSequenceError)
        rl._fragment = mal + '\n'
    else
        console.error e.stack ? e
    re-prompt rl

current-names-completer = (env, partial-line) -->
    matches = partial-line?.match /\w+$/
    return [[], partial-line] unless matches
    [current-symbol] = matches
    hits = [k for k of env when 0 is k.indexOf current-symbol]
    [hits, current-symbol]
