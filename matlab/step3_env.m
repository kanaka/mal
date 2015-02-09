function step3_env(varargin), main(varargin), end

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
    case 'let*'
        let_env = Env(env);
        for i=1:2:length(ast{2})
            let_env.set(ast{2}{i}, EVAL(ast{2}{i+1}, let_env));
        end
        ret = EVAL(ast{3}, let_env);
    otherwise
        el = eval_ast(ast, env);
        f = el{1};
        args = el(2:end);
        ret = f(args{:});
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
    repl_env.set(types.Symbol('+'), @(a,b) a+b);
    repl_env.set(types.Symbol('-'), @(a,b) a-b);
    repl_env.set(types.Symbol('*'), @(a,b) a*b);
    repl_env.set(types.Symbol('/'), @(a,b) floor(a/b));

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
