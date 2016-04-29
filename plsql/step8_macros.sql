@io.sql
@types.sql
@reader.sql
@printer.sql
@env.sql
@core.sql

CREATE OR REPLACE PACKAGE mal IS

FUNCTION MAIN(pwd varchar) RETURN integer;

END mal;
/

CREATE OR REPLACE PACKAGE BODY mal IS

FUNCTION MAIN(pwd varchar) RETURN integer IS
    M         mem_type;
    env_mem   env_mem_type;
    repl_env  integer;
    x         integer;
    line      varchar2(4000);
    core_ns   core_ns_type;
    cidx      integer;

    -- read
    FUNCTION READ(line varchar) RETURN integer IS
    BEGIN
        RETURN reader.read_str(M, line);
    END;

    -- eval

    -- forward declarations
    FUNCTION EVAL(orig_ast integer, orig_env integer) RETURN integer;
    FUNCTION do_builtin(fn integer, args mal_seq_items_type) RETURN integer;

    FUNCTION is_pair(ast integer) RETURN BOOLEAN IS
    BEGIN
        RETURN M(ast).type_id IN (8,9) AND types.count(M, ast) > 0;
    END;

    FUNCTION quasiquote(ast integer) RETURN integer IS
        a0   integer;
        a00  integer;
    BEGIN
        IF NOT is_pair(ast) THEN
            RETURN types.list(M, types.symbol(M, 'quote'), ast);
        ELSE
            a0 := types.nth(M, ast, 0);
            IF M(a0).type_id = 7 AND
               TREAT(m(a0) AS mal_str_type).val_str = 'unquote' THEN
                RETURN types.nth(M, ast, 1);
            ELSIF is_pair(a0) THEN
                a00 := types.nth(M, a0, 0);
                IF M(a00).type_id = 7 AND
                   TREAT(M(a00) AS mal_str_type).val_str = 'splice-unquote' THEN
                    RETURN types.list(M, types.symbol(M, 'concat'),
                                         types.nth(M, a0, 1),
                                         quasiquote(types.slice(M, ast, 1)));
                END IF;
            END IF;
            RETURN types.list(M, types.symbol(M, 'cons'),
                                 quasiquote(a0),
                                 quasiquote(types.slice(M, ast, 1)));
        END IF;
    END;


    FUNCTION is_macro_call(ast integer, env integer) RETURN BOOLEAN IS
        a0   integer;
        mac  integer;
    BEGIN
        IF M(ast).type_id = 8 THEN
            a0 := types.nth(M, ast, 0);
            IF M(a0).type_id = 7 AND
               env_pkg.env_find(M, env_mem, env, a0) IS NOT NULL THEN
                mac := env_pkg.env_get(M, env_mem, env, a0);
                IF M(mac).type_id = 12 THEN
                    RETURN TREAT(M(mac) AS malfunc_type).is_macro > 0;
                END IF;
            END IF;
        END IF;
        RETURN FALSE;
    END;

    FUNCTION macroexpand(orig_ast integer, env integer) RETURN integer IS
        ast     integer;
        mac     integer;
        malfn   malfunc_type;
        fargs   mal_seq_items_type;
        fn_env  integer;
    BEGIN
        ast := orig_ast;
        WHILE is_macro_call(ast, env) LOOP
            mac := env_pkg.env_get(M, env_mem, env, types.nth(M, ast, 0));
            fargs := TREAT(M(types.slice(M, ast, 1)) as mal_seq_type).val_seq;
            if M(mac).type_id = 12 THEN
                malfn := TREAT(M(mac) AS malfunc_type);
                fn_env := env_pkg.env_new(M, env_mem, malfn.env,
                                          malfn.params,
                                          fargs);
                ast := EVAL(malfn.ast, fn_env);
            ELSE
                ast := do_builtin(mac, fargs);
            END IF;
        END LOOP;
        RETURN ast;
    END;

    FUNCTION eval_ast(ast integer, env integer) RETURN integer IS
        i        integer;
        old_seq  mal_seq_items_type;
        new_seq  mal_seq_items_type;
    BEGIN
        IF M(ast).type_id = 7 THEN
            RETURN env_pkg.env_get(M, env_mem, env, ast);
        ELSIF M(ast).type_id IN (8,9) THEN
            old_seq := TREAT(M(ast) AS mal_seq_type).val_seq;
            new_seq := mal_seq_items_type();
            new_seq.EXTEND(old_seq.COUNT);
            FOR i IN 1..old_seq.COUNT LOOP
                new_seq(i) := EVAL(old_seq(i), env);
            END LOOP;
            RETURN types.seq(M, M(ast).type_id, new_seq);
        ELSE
            RETURN ast;
        END IF;
    END;

    FUNCTION EVAL(orig_ast integer, orig_env integer) RETURN integer IS
        ast      integer := orig_ast;
        env      integer := orig_env;
        el       integer;
        a0       integer;
        a0sym    varchar2(100);
        seq      mal_seq_items_type;
        let_env  integer;
        i        integer;
        f        integer;
        cond     integer;
        malfn    malfunc_type;
        args     mal_seq_items_type;
    BEGIN
      WHILE TRUE LOOP
        IF M(ast).type_id <> 8 THEN
            RETURN eval_ast(ast, env);
        END IF;

        -- apply
        ast := macroexpand(ast, env);
        IF M(ast).type_id <> 8 THEN
            RETURN eval_ast(ast, env);
        END IF;
        IF types.count(M, ast) = 0 THEN
            RETURN ast;
        END IF;

        a0 := types.first(M, ast);
        if M(a0).type_id = 7 THEN -- symbol
            a0sym := TREAT(M(a0) AS mal_str_type).val_str;
        ELSE
            a0sym := '__<*fn*>__';
        END IF;

        CASE
        WHEN a0sym = 'def!' THEN
            RETURN env_pkg.env_set(M, env_mem, env,
                types.nth(M, ast, 1), EVAL(types.nth(M, ast, 2), env));
        WHEN a0sym = 'let*' THEN
            let_env := env_pkg.env_new(M, env_mem, env);
            seq := TREAT(M(types.nth(M, ast, 1)) AS mal_seq_type).val_seq;
            i := 1;
            WHILE i <= seq.COUNT LOOP
                x := env_pkg.env_set(M, env_mem, let_env,
                    seq(i), EVAL(seq(i+1), let_env));
                i := i + 2;
            END LOOP;
            env := let_env;
            ast := types.nth(M, ast, 2); -- TCO
        WHEN a0sym = 'quote' THEN
            RETURN types.nth(M, ast, 1);
        WHEN a0sym = 'quasiquote' THEN
            RETURN EVAL(quasiquote(types.nth(M, ast, 1)), env);
        WHEN a0sym = 'defmacro!' THEN
            x := EVAL(types.nth(M, ast, 2), env);
            malfn := TREAT(M(x) as malfunc_type);
            malfn.is_macro := 1;
            M(x) := malfn;
            RETURN env_pkg.env_set(M, env_mem, env,
                types.nth(M, ast, 1), x);
        WHEN a0sym = 'macroexpand' THEN
            RETURN macroexpand(types.nth(M, ast, 1), env);
        WHEN a0sym = 'do' THEN
            x := types.slice(M, ast, 1, types.count(M, ast)-2);
            x := eval_ast(x, env);
            ast := types.nth(M, ast, types.count(M, ast)-1);  -- TCO
        WHEN a0sym = 'if' THEN
            cond := EVAL(types.nth(M, ast, 1), env);
            IF cond = 1 OR cond = 2 THEN  -- nil or false
                IF types.count(M, ast) > 3 THEN
                    ast := EVAL(types.nth(M, ast, 3), env);  -- TCO
                ELSE
                    RETURN 1;  -- nil
                END IF;
            ELSE
                ast := EVAL(types.nth(M, ast, 2), env);  -- TCO
            END IF;
        WHEN a0sym = 'fn*' THEN
            RETURN types.malfunc(M, types.nth(M, ast, 2),
                                    types.nth(M, ast, 1),
                                    env);
        ELSE
            el := eval_ast(ast, env);
            f := types.first(M, el);
            args := TREAT(M(types.slice(M, el, 1)) AS mal_seq_type).val_seq;
            IF M(f).type_id = 12 THEN
                malfn := TREAT(M(f) AS malfunc_type);
                env := env_pkg.env_new(M, env_mem, malfn.env,
                                          malfn.params, args);
                ast := malfn.ast;  -- TCO
            ELSE
                RETURN do_builtin(f, args);
            END IF;
        END CASE;

      END LOOP;

    END;

    -- hack to get around lack of function references
    -- functions that require special access to repl_env or EVAL
    -- are implemented directly here, otherwise, core.do_core_fn
    -- is called.
    FUNCTION do_builtin(fn integer, args mal_seq_items_type) RETURN integer IS
        fname   varchar2(100);
        val     integer;
        f       integer;
        malfn   malfunc_type;
        fargs   mal_seq_items_type;
        fn_env  integer;
    BEGIN
        fname := TREAT(M(fn) AS mal_str_type).val_str;
        CASE
        WHEN fname = 'do_eval' THEN
            RETURN EVAL(args(1), repl_env);
        WHEN fname = 'swap!' THEN
            val := TREAT(M(args(1)) AS mal_atom_type).val;
            f := args(2);
            -- slice one extra at the beginning that will be changed
            -- to the value of the atom
            fargs := TREAT(M(types.slice(M, args, 1)) AS mal_seq_type).val_seq;
            fargs(1) := val;
            IF M(f).type_id = 12 THEN
                malfn := TREAT(M(f) AS malfunc_type);
                fn_env := env_pkg.env_new(M, env_mem, malfn.env,
                                          malfn.params, fargs);
                val := EVAL(malfn.ast, fn_env);
            ELSE
                val := do_builtin(f, fargs);
            END IF;
            M(args(1)) := mal_atom_type(13, val);
            RETURN val;
        ELSE
            RETURN core.do_core_func(M, fn, args);
        END CASE;
    END;


    -- print
    FUNCTION PRINT(exp integer) RETURN varchar IS
    BEGIN
        RETURN printer.pr_str(M, exp);
    END;

    -- repl
    FUNCTION REP(line varchar) RETURN varchar IS
    BEGIN
        RETURN PRINT(EVAL(READ(line), repl_env));
    END;

BEGIN
    M := types.mem_new();
    env_mem := env_mem_type();

    repl_env := env_pkg.env_new(M, env_mem, NULL);

    -- core.EXT: defined using PL/SQL
    core_ns := core.get_core_ns();
    FOR cidx IN 1..core_ns.COUNT LOOP
        x := env_pkg.env_set(M, env_mem, repl_env,
            types.symbol(M, core_ns(cidx)),
            types.func(M, core_ns(cidx)));
    END LOOP;
    x := env_pkg.env_set(M, env_mem, repl_env,
        types.symbol(M, 'eval'),
        types.func(M, 'do_eval'));
    x := env_pkg.env_set(M, env_mem, repl_env,
        types.symbol(M, '*ARGV*'),
        types.list(M));

    -- core.mal: defined using the language itself
    line := REP('(def! not (fn* (a) (if a false true)))');
    line := REP('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) ")")))))');
    line := REP('(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list ''if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons ''cond (rest (rest xs)))))))');
    line := REP('(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))');

    WHILE true LOOP
        BEGIN
            line := stream_readline('user> ', 0);
            IF line IS NULL THEN CONTINUE; END IF;
            IF line IS NOT NULL THEN
                stream_writeline(REP(line));
            END IF;

            EXCEPTION WHEN OTHERS THEN
                IF SQLCODE = -20001 THEN  -- io streams closed
                    RETURN 0;
                END IF;
                stream_writeline('Error: ' || SQLERRM);
                stream_writeline(dbms_utility.format_error_backtrace);
        END;
    END LOOP;
END;

END mal;
/
show errors;

quit;
