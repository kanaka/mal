function step9_try(varargin), main(varargin), end

% read
function ret = READ(str)
    ret = reader.read_str(str);
end

% eval
function ret = is_pair(ast)
    ret = type_utils.sequential_Q(ast) && length(ast) > 0;
end

function ret = quasiquote(ast)
    if ~is_pair(ast)
        ret = types.List(types.Symbol('quote'), ast);
    elseif isa(ast.get(1),'types.Symbol') && ...
           strcmp(ast.get(1).name, 'unquote')
        ret = ast.get(2);
    elseif is_pair(ast.get(1)) && ...
           isa(ast.get(1).get(1),'types.Symbol') && ...
           strcmp(ast.get(1).get(1).name, 'splice-unquote')
        ret = types.List(types.Symbol('concat'), ...
                         ast.get(1).get(2), ...
                         quasiquote(ast.slice(2)));
    else
        ret = types.List(types.Symbol('cons'), ...
                         quasiquote(ast.get(1)), ...
                         quasiquote(ast.slice(2)));
    end
end

function ret = is_macro_call(ast, env)
    if type_utils.list_Q(ast) && isa(ast.get(1), 'types.Symbol') && ...
       ~islogical(env.find(ast.get(1)))
        f = env.get(ast.get(1));
        ret = isa(f,'types.Function') && f.is_macro;
    else
        ret = false;
    end
end

function ret = macroexpand(ast, env)
    while is_macro_call(ast, env)
        mac = env.get(ast.get(1));
        args = ast.slice(2);
        ast = mac.fn(args.data{:});
    end
    ret = ast;
end

function ret = eval_ast(ast, env)
    switch class(ast)
    case 'types.Symbol'
        ret = env.get(ast);
    case 'types.List'
        ret = types.List();
        for i=1:length(ast)
            ret.append(EVAL(ast.get(i), env));
        end
    case 'types.Vector'
        ret = types.Vector();
        for i=1:length(ast)
            ret.append(EVAL(ast.get(i), env));
        end
    case 'types.HashMap'
        ret = types.HashMap();
        ks = ast.keys();
        for i=1:length(ks)
            k = ks{i};
            ret.set(EVAL(k, env), EVAL(ast.get(k), env));
        end
    otherwise
        ret = ast;
    end
end

function ret = EVAL(ast, env)
  while true
    %fprintf('EVAL: %s\n', printer.pr_str(ast, true));
    if ~type_utils.list_Q(ast)
        ret = eval_ast(ast, env);
        return;
    end

    % apply
    ast = macroexpand(ast, env);
    if ~type_utils.list_Q(ast)
        ret = eval_ast(ast, env);
        return;
    end

    if isa(ast.get(1),'types.Symbol')
        a1sym = ast.get(1).name;
    else
        a1sym = '_@$fn$@_';
    end
    switch (a1sym)
    case 'def!'
        ret = env.set(ast.get(2), EVAL(ast.get(3), env));
        return;
    case 'let*'
        let_env = Env({env});
        for i=1:2:length(ast.get(2))
            let_env.set(ast.get(2).get(i), EVAL(ast.get(2).get(i+1), let_env));
        end
        env = let_env;
        ast = ast.get(3); % TCO
    case 'quote'
        ret = ast.get(2);
        return;
    case 'quasiquote'
        ast = quasiquote(ast.get(2)); % TCO
    case 'defmacro!'
        ret = env.set(ast.get(2), EVAL(ast.get(3), env));
        ret.is_macro = true;
        return;
    case 'macroexpand'
        ret = macroexpand(ast.get(2), env);
        return;
    case 'try*'
        try
            ret = EVAL(ast.get(2), env);
            return;
        catch e
            if length(ast) > 2 && strcmp(ast.get(3).get(1).name, 'catch*')
                if strcmp(e.identifier, 'MalException:object')
                    if exist('OCTAVE_VERSION', 'builtin') ~= 0
                        global error_object;
                        exc = error_object;
                    else
                        exc = e.obj;
                    end
                else
                    exc = e.message;
                end
                catch_env = Env({env}, types.List(ast.get(3).get(2)), ...
                                types.List(exc));
                ret = EVAL(ast.get(3).get(3), catch_env);
                return;
            else
                throw(e);
            end
        end
    case 'do'
        el = eval_ast(ast.slice(2,length(ast)-1), env);
        ast = ast.get(length(ast)); % TCO
    case 'if'
        cond = EVAL(ast.get(2), env);
        if strcmp(class(cond), 'types.Nil') || ...
           (islogical(cond) && cond == false)
           if length(ast) > 3
               ast = ast.get(4); % TCO
            else
               ret = type_utils.nil;
               return;
            end
        else
            ast = ast.get(3); % TCO
        end
    case 'fn*'
        fn = @(varargin) EVAL(ast.get(3), Env({env}, ast.get(2), ...
                                              types.List(varargin{:})));
        ret = types.Function(fn, ast.get(3), env, ast.get(2));
        return;
    otherwise
        el = eval_ast(ast, env);
        f = el.get(1);
        args = el.slice(2);
        if isa(f, 'types.Function')
            env = Env({f.env}, f.params, args);
            ast = f.ast; % TCO
        else
            ret = f(args.data{:});
            return
        end
    end
  end
end

% print
function ret = PRINT(ast)
    ret = printer.pr_str(ast, true);
end

% REPL
function ret = rep(str, env)
    ret = PRINT(EVAL(READ(str), env));
end

function main(args)
    repl_env = Env();

    % core.m: defined using matlab
    ns = core.ns(); ks = ns.keys();
    for i=1:length(ks)
        k = ks{i};
        repl_env.set(types.Symbol(k), ns(k));
    end
    repl_env.set(types.Symbol('eval'), @(a) EVAL(a, repl_env));
    rest_args = args(2:end);
    repl_env.set(types.Symbol('*ARGV*'), types.List(rest_args{:}));

    % core.mal: defined using the langauge itself
    rep('(def! not (fn* (a) (if a false true)))', repl_env);
    rep('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) ")")))))"', repl_env);
    rep('(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list ''if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons ''cond (rest (rest xs)))))))', repl_env);
    rep('(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))', repl_env);

    if ~isempty(args)
        rep(sprintf('(load-file "%s")', args{1}), repl_env);
        quit;
    end

    %cleanObj = onCleanup(@() disp('*** here1 ***'));
    while (true)
        try
            line = input('user> ', 's');
        catch err
            return
        end
        if strcmp(strtrim(line),''), continue, end
        try
            fprintf('%s\n', rep(line, repl_env));
        catch err
            if strcmp('MalException:object', err.identifier)
                if exist('OCTAVE_VERSION', 'builtin') ~= 0
                    global error_object;
                    fprintf('Error: %s\n', printer.pr_str(error_object, true));
                else
                    fprintf('Error: %s\n', printer.pr_str(err.obj, true));
                end
            else
                fprintf('Error: %s\n', err.message);
            end
            type_utils.print_stack(err);
        end
    end
end
