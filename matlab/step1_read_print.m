function step1_read_print(varargin), main(varargin), end

function ret = READ(str)
    ret = reader.read_str(str);
end

function ret = EVAL(ast, env)
    ret = ast;
end

function ret = PRINT(ast)
    ret = printer.pr_str(ast, true);
end

function ret = rep(str)
    ret = PRINT(EVAL(READ(str), ''));
end

function main(args)
    %cleanObj = onCleanup(@() disp('*** here1 ***'));
    while (true)
        line = input('user> ', 's');
        if strcmp(strtrim(line),''), continue, end
        try
            fprintf('%s\n', rep(line));
        catch err
            fprintf('Error: %s\n', err.message);
            fprintf('%s\n', getReport(err, 'extended'));
        end
    end
end
