function step0_repl(varargin), main(varargin), end

function ret = READ(str)
    ret = str;
end

function ret = EVAL(ast, env)
    ret = ast;
end

function ret = PRINT(ast)
    ret = ast;
end

function ret = rep(str)
    ret = PRINT(EVAL(READ(str), ''));
end

function main(args)
    while (true)
        line = input('user> ', 's');
        fprintf('%s\n', rep(line));
    end
end

