function step0_repl(varargin), main(varargin), end

% read
function ret = READ(str)
    ret = str;
end

% eval
function ret = EVAL(ast, env)
    ret = ast;
end

% print
function ret = PRINT(ast)
    ret = ast;
end

% REPL
function ret = rep(str, env)
    ret = PRINT(EVAL(READ(str), env));
end

function main(args)
    while (true)
        line = input('user> ', 's');
        fprintf('%s\n', rep(line, ''));
    end
end
