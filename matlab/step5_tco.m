function step5_tco(varargin), main(varargin), end

% read
function ret = READ(str)
    ret = reader.read_str(str);
end

% eval
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

    % core.mal: defined using the langauge itself
    rep('(def! not (fn* (a) (if a false true)))', repl_env);

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
