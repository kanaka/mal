require_relative "mal_readline"
require_relative "types"
require_relative "reader"
require_relative "printer"
require_relative "env"
require_relative "core"

# read
def READ(str)
    return read_str(str)
end

# eval
def starts_with(ast, sym)
    return ast.is_a?(List) && ast.size == 2 && ast[0] == sym
end

def qq_loop(ast)
  acc = List.new []
  ast.reverse_each do |elt|
    if starts_with(elt, :"splice-unquote")
      acc = List.new [:concat, elt[1], acc]
    else
      acc = List.new [:cons, quasiquote(elt), acc]
    end
  end
  return acc
end

def quasiquote(ast)
  return case ast
  when List
    if starts_with(ast, :unquote)
      ast[1]
    else
      qq_loop(ast)
    end
  when Vector
    List.new [:vec, qq_loop(ast)]
  when Hash
    List.new [:quote, ast]
  when Symbol
    List.new [:quote, ast]
  else
    ast
  end
end

def macro_call?(ast, env)
    return (ast.is_a?(List) &&
            ast[0].is_a?(Symbol) &&
            env.find(ast[0]) &&
            env.get(ast[0]).is_a?(Function) &&
            env.get(ast[0]).is_macro)
end

def macroexpand(ast, env)
    while macro_call?(ast, env)
        mac = env.get(ast[0])
        ast = mac[*ast.drop(1)]
    end
    return ast
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
    ast = macroexpand(ast, env)
    if not ast.is_a? List
        return eval_ast(ast, env)
    end
    if ast.empty?
        return ast
    end

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
    when :quasiquoteexpand
        return quasiquote(a1);
    when :quasiquote
        ast = quasiquote(a1); # Continue loop (TCO)
    when :defmacro!
        func = EVAL(a2, env).clone
        func.is_macro = true
        return env.set(a1, func)
    when :macroexpand
        return macroexpand(a1, env)
    when :"rb*"
        res = eval(a1)
        return case res
            when Array; List.new res
            else; res
        end
    when :"try*"
        begin
            return EVAL(a1, env)
        rescue Exception => exc
            if exc.is_a? MalException
                exc = exc.data
            else
                exc = exc.message
            end
            if a2 && a2[0] == :"catch*"
                return EVAL(a2[2], Env.new(env, [a2[1]], [exc]))
            else
                raise exc
            end
        end
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
            EVAL(a2, Env.new(env, a1, List.new(args)))
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
RE["(def! *host-language* \"ruby\")"]
RE["(def! not (fn* (a) (if a false true)))"]
RE["(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"]
RE["(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"]

if ARGV.size > 0
    RE["(load-file \"" + ARGV[0] + "\")"]
    exit 0
end

# repl loop
RE["(println (str \"Mal [\" *host-language* \"]\"))"]
while line = _readline("user> ")
    begin
        puts REP[line]
    rescue Exception => e
        if e.is_a? MalException
            puts "Error: #{_pr_str(e.data, true)}" 
        else
            puts "Error: #{e}" 
        end
        puts "\t#{e.backtrace.join("\n\t")}"
    end
end
