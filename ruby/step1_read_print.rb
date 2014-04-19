$: << File.expand_path(File.dirname(__FILE__))
require "mal_readline"
require "types"
require "reader"
require "printer"

# read
def READ(str)
    return read_str(str)
end

# eval
def EVAL(ast, env)
    return ast
end

# print
def PRINT(exp)
    return _pr_str(exp, true)
end

# repl
def REP(str)
    return PRINT(EVAL(READ(str), {}))
end

# repl loop
while line = _readline("user> ")
    begin
        puts REP(line)
    rescue Exception => e
        puts "Error: #{e}" 
        puts "\t#{e.backtrace.join("\n\t")}"
    end
end
