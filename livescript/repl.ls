require! {
    readline
}

{stdin, stdout} = process

USER_IS_HUMAN = !!(process.env.HUMAN or process.env.npm_package_config_human)

export run-repl = (rep) ->
    rl = readline.create-interface terminal: USER_IS_HUMAN, input: stdin, output: stdout
    rl.set-prompt 'user> '

    rl.on \line, (mal) ->
        try
            ret = rep mal
            console.log(ret) if ret?
        catch e
            console.error e
            if e?.stack
                console.error e.stack

        stdin.write 'user> '
        # rl.prompt()

    rl.on \close, ->
        console.log '\nGoodbye!'
        process.exit!

    rl.prompt()
