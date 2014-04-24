$: << File.expand_path(File.dirname(__FILE__))
require "mal_readline"
require "types"
require "reader"
require "printer"
require "env"
require "core"

# read
def READ(str)
    return read_str(str)
end

# eval
def pair?(x)
    return sequential?(x) && x.size > 0
end

def quasiquote(ast)
    if not pair?(ast)
        return List.new [:quote, ast]
    elsif ast[0] == :unquote
        return ast[1]
    elsif pair?(ast[0]) && ast[0][0] == :"splice-unquote"
        return List.new [:concat, ast[0][1], quasiquote(ast.drop(1))]
    else
        return List.new [:cons, quasiquote(ast[0]), quasiquote(ast.drop(1))]
    end
end

def eval_ast(ast, env)
    return case ast
        when Symbol
            env.get(ast)
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
    while true

    #puts "EVAL: #{_pr_str(ast, true)}"

    if not ast.is_a? List
        return eval_ast(ast, env)
    end

    # apply list
    a0,a1,a2,a3 = ast
    case a0
    when :def!
        return env.set(a1, EVAL(a2, env))
    when :"let*"
        let_env = Env.new(env)
        a1.each_slice(2) do |a,e|
            let_env.set(a, EVAL(e, let_env))
        end
        env = let_env
        ast = a2 # Continue loop (TCO)
    when :quote
        return a1
    when :quasiquote
        ast = quasiquote(a1); # Continue loop (TCO)
    when :do
        eval_ast(ast[1..-2], env)
        ast = ast.last # Continue loop (TCO)
    when :if
        cond = EVAL(a1, env)
        if not cond
            return nil if a3 == nil
            ast = a3 # Continue loop (TCO)
        else
            ast = a2 # Continue loop (TCO)
        end
    when :"fn*"
        return Function.new(a2, env, a1) {|*args|
            EVAL(a2, Env.new(env, a1, args))
        }
    else
        el = eval_ast(ast, env)
        f = el[0]
        if f.class == Function
            ast = f.ast
            env = f.gen_env(el.drop(1)) # Continue loop (TCO)
        else
            return f[*el.drop(1)]
        end
    end

    end
end

# print
def PRINT(exp)
    return _pr_str(exp, true)
end

# repl
repl_env = Env.new
RE = lambda {|str| EVAL(READ(str), repl_env) }
REP = lambda {|str| PRINT(EVAL(READ(str), repl_env)) }

# core.rb: defined using ruby
$core_ns.each do |k,v| repl_env.set(k,v) end
repl_env.set(:eval, lambda {|ast| EVAL(ast, repl_env)})
repl_env.set(:"*ARGV*", List.new(ARGV.slice(1,ARGV.length) || []))

# core.mal: defined using the language itself
RE["(def! not (fn* (a) (if a false true)))"]
RE["(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"]

if ARGV.size > 0
    RE["(load-file \"" + ARGV[0] + "\")"]
    exit 0
end

# repl loop
while line = _readline("user> ")
    begin
        puts REP[line]
    rescue Exception => e
        puts "Error: #{e}" 
        puts "\t#{e.backtrace.join("\n\t")}"
    end
end
