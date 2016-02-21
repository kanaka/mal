module readline_mod

export do_readline

function do_readline(prompt)
    print(prompt)
    flush(STDOUT)
    line = readline(STDIN)
    if line == ""
        return nothing
    end
    chomp(line)
end

end
