function step2_eval(varargin), main(varargin), end

% read
function ret = READ(str)
    ret = reader.read_str(str);
end

% eval
function ret = EVAL(ast, env)

  % fprintf('EVAL: %s\n', printer.pr_str(ast, true));

    switch class(ast)
    case 'types.Symbol'
        ret = env(ast.name);
        return;
    case 'types.List'
        %  Proceed after this switch.
    case 'types.Vector'
        ret = types.Vector();
        for i=1:length(ast)
            ret.append(EVAL(ast.get(i), env));
        end
        return;
    case 'types.HashMap'
        ret = types.HashMap();
        ks = ast.keys();
        for i=1:length(ks)
            k = ks{i};
            ret.set(k, EVAL(ast.get(k), env));
        end
        return;
    otherwise
        ret = ast;
        return;
    end

    % apply
    if length(ast) == 0
        ret = ast;
        return;
    end

       f = EVAL(ast.get(1), env);
        args = types.List();
        for i=2:length(ast)
            args.append(EVAL(ast.get(i), env));
        end
        ret = f(args.data{:});

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
    if exist('OCTAVE_VERSION', 'builtin') ~= 0
        repl_env = Dict();
    else
        repl_env = containers.Map();
    end
    repl_env('+') = @(a,b) a+b;
    repl_env('-') = @(a,b) a-b;
    repl_env('*') = @(a,b) a*b;
    repl_env('/') = @(a,b) floor(a/b);

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
            fprintf('Error: %s\n', err.message);
            type_utils.print_stack(err);
        end
    end
end
