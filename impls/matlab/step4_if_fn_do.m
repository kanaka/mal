function step4_if_fn_do(varargin), main(varargin), end

% read
function ret = READ(str)
    ret = reader.read_str(str);
end

% eval
function ret = EVAL(ast, env)

    dbgeval = env.get('DEBUG-EVAL');
    if ~isequal(dbgeval, {}) ...
       && ~strcmp(class(dbgeval), 'types.Nil') ...
       && (~islogical(dbgeval) || dbgeval)
      fprintf('EVAL: %s\n', printer.pr_str(ast, true));
    end

    switch class(ast)
    case 'types.Symbol'
        ret = env.get(ast.name);
        if isequal(ret, {})
            msg = sprintf('''%s'' not found', ast.name);
            if exist('OCTAVE_VERSION', 'builtin') ~= 0
                error('ENV:notfound', msg);
            else
                throw(MException('ENV:notfound', msg));
            end
        end
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

    if isa(ast.get(1),'types.Symbol')
        a1sym = ast.get(1).name;
    else
        a1sym = '_@$fn$@_';
    end
    switch (a1sym)
    case 'def!'
        ret = env.set(ast.get(2), EVAL(ast.get(3), env));
    case 'let*'
        let_env = Env({env});
        for i=1:2:length(ast.get(2))
            let_env.set(ast.get(2).get(i), EVAL(ast.get(2).get(i+1), let_env));
        end
        ret = EVAL(ast.get(3), let_env);
    case 'do'
        for i=2:length(ast)
            ret = EVAL(ast.get(i), env);
        end
    case 'if'
        cond = EVAL(ast.get(2), env);
        if strcmp(class(cond), 'types.Nil') || ...
           (islogical(cond) && cond == false)
           if length(ast) > 3
               ret = EVAL(ast.get(4), env);
            else
               ret = type_utils.nil;
            end
        else
            ret = EVAL(ast.get(3), env);
        end
    case 'fn*'
        ret = @(varargin) EVAL(ast.get(3), Env({env}, ast.get(2), ...
                                               types.List(varargin{:})));
    otherwise
       f = EVAL(ast.get(1), env);
        args = types.List();
        for i=2:length(ast)
            args.append(EVAL(ast.get(i), env));
        end
        ret = f(args.data{:});
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

    % core.mal: defined using the langauge itself
    rep('(def! not (fn* (a) (if a false true)))', repl_env);

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
