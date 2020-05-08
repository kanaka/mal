require_relative "mal_readline"
require_relative "types"
require_relative "reader"
require_relative "printer"

# read
def READ(str)
    return read_str(str)
end

# eval
def eval_ast(ast, env)
    return case ast
        when Symbol
            raise "'" + ast.to_s + "' not found" if not env.key? ast
            env[ast]
        when List   
            List.new ast.map{|a| EVAL(a, env)}
        when Vector
            Vector.new ast.map{|a| EVAL(a, env)}
        when Hash
            new_hm = {}
            ast.each{|k,v| new_hm[EVAL(k,env)] = EVAL(v, env)}
            new_hm
        else 
            ast
    end
end

def EVAL(ast, env)
    #puts "EVAL: #{_pr_str(ast, true)}"

    if not ast.is_a? List
        return eval_ast(ast, env)
    end
    if ast.empty?
        return ast
    end

    # apply list
    el = eval_ast(ast, env)
    f = el[0]
    return f[*el.drop(1)]
end

# print
def PRINT(exp)
    return _pr_str(exp, true)
end

# repl
repl_env = {}
REP = lambda {|str| PRINT(EVAL(READ(str), repl_env)) }

repl_env[:+] = lambda {|a,b| a + b}
repl_env[:-] = lambda {|a,b| a - b}
repl_env[:*] = lambda {|a,b| a * b}
repl_env[:/] = lambda {|a,b| a / b}

# repl loop
while line = _readline("user> ")
    begin
        puts REP[line]
    rescue Exception => e
        puts "Error: #{e}" 
        puts "\t#{e.backtrace.join("\n\t")}"
    end
end
