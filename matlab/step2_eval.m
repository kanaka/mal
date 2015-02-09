function step2_eval(varargin), main(varargin), end

% read
function ret = READ(str)
    ret = reader.read_str(str);
end

% eval
function ret = eval_ast(ast, env)
    switch class(ast)
    case 'types.Symbol'
        ret = env(ast.name);
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
    el = eval_ast(ast, env);
    f = el{1};
    args = el(2:end);
    ret = f(args{:});
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
    repl_env = containers.Map();
    repl_env('+') = @(a,b) a+b;
    repl_env('-') = @(a,b) a-b;
    repl_env('*') = @(a,b) a*b;
    repl_env('/') = @(a,b) floor(a/b);

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
