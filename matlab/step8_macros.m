function step8_macros(varargin), main(varargin), end

% read
function ret = READ(str)
    ret = reader.read_str(str);
end

% eval
function ret = is_pair(ast)
    ret = iscell(ast) && length(ast) > 0;
end

function ret = quasiquote(ast)
    if ~is_pair(ast)
        ret = {types.Symbol('quote'), ast};
    elseif isa(ast{1},'types.Symbol') && ...
           strcmp(ast{1}.name, 'unquote')
        ret = ast{2};
    elseif is_pair(ast{1}) && isa(ast{1}{1},'types.Symbol') && ...
           strcmp(ast{1}{1}.name, 'splice-unquote')
        ret = {types.Symbol('concat'), ...
               ast{1}{2}, ...
               quasiquote(ast(2:end))};
    else
        ret = {types.Symbol('cons'), ...
               quasiquote(ast{1}), ...
               quasiquote(ast(2:end))};
    end
end

function ret = is_macro_call(ast, env)
    if iscell(ast) && isa(ast{1}, 'types.Symbol') && ...
       ~islogical(env.find(ast{1}))
        f = env.get(ast{1});
        ret = isa(f,'Function') && f.is_macro;
    else
        ret = false;
    end
end

function ret = macroexpand(ast, env)
    while is_macro_call(ast, env)
        mac = env.get(ast{1});
        ast = mac.fn(ast{2:end});
    end
    ret = ast;
end

function ret = eval_ast(ast, env)
    switch class(ast)
    case 'types.Symbol'
        ret = env.get(ast);
    case 'cell'
        ret = {};
        for i=1:length(ast)
            ret{end+1} = EVAL(ast{i}, env);
        end
    otherwise
        ret = ast;
    end
end

function ret = EVAL(ast, env)
  while true
    if ~iscell(ast)
        ret = eval_ast(ast, env);
        return;
    end

    % apply
    ast = macroexpand(ast, env);
    if ~iscell(ast)
        ret = ast;
        return;
    end

    if isa(ast{1},'types.Symbol')
        a1sym = ast{1}.name;
    else
        a1sym = '_@$fn$@_';
    end
    switch (a1sym)
    case 'def!'
        ret = env.set(ast{2}, EVAL(ast{3}, env));
        return;
    case 'let*'
        let_env = Env(env);
        for i=1:2:length(ast{2})
            let_env.set(ast{2}{i}, EVAL(ast{2}{i+1}, let_env));
        end
        env = let_env;
        ast = ast{3}; % TCO
    case 'quote'
        ret = ast{2};
        return;
    case 'quasiquote'
        ast = quasiquote(ast{2}); % TCO
    case 'defmacro!'
        ret = env.set(ast{2}, EVAL(ast{3}, env));
        ret.is_macro = true;
        return;
    case 'macroexpand'
        ret = macroexpand(ast{2}, env);
        return;
    case 'do'
        el = eval_ast(ast(2:end-1), env);
        ast = ast{end}; % TCO
    case 'if'
        cond = EVAL(ast{2}, env);
        if strcmp(class(cond), 'types.Nil') || ...
           (islogical(cond) && cond == false)
           if length(ast) > 3
               ast = ast{4}; % TCO
            else
               ret = types.nil;
               return;
            end
        else
            ast = ast{3}; % TCO
        end
    case 'fn*'
        fn = @(varargin) EVAL(ast{3}, Env(env, ast{2}, varargin));
        ret = Function(fn, ast{3}, env, ast{2});
        return;
    otherwise
        el = eval_ast(ast, env);
        f = el{1};
        args = el(2:end);
        if isa(f, 'Function')
            env = Env(f.env, f.params, args);
            ast = f.ast; % TCO
        else
            ret = f(args{:});
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
    repl_env = Env(false);

    % core.m: defined using matlab
    ns = core.ns(); ks = ns.keys();
    for i=1:length(ks)
        k = ks{i};
        repl_env.set(types.Symbol(k), ns(k));
    end
    repl_env.set(types.Symbol('eval'), @(a) EVAL(a, repl_env));
    repl_env.set(types.Symbol('*ARGV*'), args(2:end));

    % core.mal: defined using the langauge itself
    rep('(def! not (fn* (a) (if a false true)))', repl_env);
    rep('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) ")")))))"', repl_env);
    rep('(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list ''if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons ''cond (rest (rest xs)))))))', repl_env);
    rep('(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))', repl_env);

    if ~isempty(args)
        rep(strcat('(load-file "', args{1}, '")'), repl_env);
        quit;
    end

    %cleanObj = onCleanup(@() disp('*** here1 ***'));
    while (true)
        line = input('user> ', 's');
        if strcmp(strtrim(line),''), continue, end
        try
            fprintf('%s\n', rep(line, repl_env));
        catch err
            fprintf('Error: %s\n', err.message);
            fprintf('%s\n', getReport(err, 'extended'));
        end
    end
end
