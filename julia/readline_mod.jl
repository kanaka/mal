module readline_mod

export do_readline

function do_readline(prompt)
    print(prompt)
    flush(stdout)
    line = readline(stdin)
    if line == ""
        return nothing
    end
    chomp(line)
end

end
