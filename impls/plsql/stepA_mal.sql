@io.sql
@types.sql
@reader.sql
@printer.sql
@env.sql
@core.sql

CREATE OR REPLACE PACKAGE mal IS

FUNCTION MAIN(args varchar DEFAULT '()') RETURN integer;

END mal;
/

CREATE OR REPLACE PACKAGE BODY mal IS

FUNCTION MAIN(args varchar DEFAULT '()') RETURN integer IS
    M         types.mal_table;                 -- general mal value memory pool
    H         types.map_entry_table;    -- hashmap memory pool
    E         env_pkg.env_entry_table;  -- mal env memory pool
    repl_env  integer;
    x         integer;
    line      CLOB;
    core_ns   core_ns_T;
    cidx      integer;
    argv      mal_vals;
    err_val   integer;

    -- read
    FUNCTION READ(line varchar) RETURN integer IS
    BEGIN
        RETURN reader.read_str(M, H, line);
    END;

    -- eval

    -- forward declarations
    FUNCTION EVAL(orig_ast integer, orig_env integer) RETURN integer;
    FUNCTION do_builtin(fn integer, args mal_vals) RETURN integer;

    FUNCTION starts_with(lst integer, sym varchar) RETURNS BOOLEAN IS
        a0 integer;
    BEGIN
        IF TREAT(M(lst) AS mal_seq_T).val_seq.COUNT = 2 THEN
            a0 := types.nth(M, ast, 0)
            RETURN M(a0).type_id = 7 AND TREAT(M(a0) AS mal_str_T).val_str = sym;
        END IF;
        RETURN FALSE;
    END;

    FUNCTION qq_loop(elt integer, acc integer) RETURNS integer IS
    BEGIN
        IF M(elt).type_id = 8 AND starts_with(elt, 'splice-unquote') THEN
            RETURN types._list(M, types.symbol('concat'), types.nth(M, a0, 1), acc);
        END IF;
        RETURN types.list(M, types.symbol('cons'), quasiquote(elt), acc);
    END;

    FUNCTION qq_foldr(xs integer[]) RETURNS integer IS
        acc integer := types.list(M);
    BEGIN
        FOR i IN REVERSE 0 .. types._count(xs) - 1 LOOP
            acc := qq_loop(types.nth(M, xs, i), acc);
        END LOOP;
        RETURN acc;
    END;

    FUNCTION quasiquote(ast integer) RETURNS integer IS
    BEGIN
        CASE
        WHEN M(ast).type_id IN (7, 10) THEN
            RETURN types.list(M, types.symbol('quote'), ast);
        WHEN M(ast).type_id = 9 THEN
            RETURN types._list(types.symbol('vec'), qq_folr(ast));
        WHEN M(ast).type_id /= 8 THEN
            RETURN ast;
        WHEN starts_with(ast, 'unquote') THEN
            RETURN types.nth(M, ast, 1);
        ELSE
            RETURN qq_foldr(ast);
        END CASE;
    END; $$ LANGUAGE plpgsql;

    FUNCTION is_macro_call(ast integer, env integer) RETURN BOOLEAN IS
        a0   integer;
        mac  integer;
    BEGIN
        IF M(ast).type_id = 8 THEN
            a0 := types.nth(M, ast, 0);
            IF M(a0).type_id = 7 AND
               env_pkg.env_find(M, E, env, a0) IS NOT NULL THEN
                mac := env_pkg.env_get(M, E, env, a0);
                IF M(mac).type_id = 12 THEN
                    RETURN TREAT(M(mac) AS mal_func_T).is_macro > 0;
                END IF;
            END IF;
        END IF;
        RETURN FALSE;
    END;

    FUNCTION macroexpand(orig_ast integer, env integer) RETURN integer IS
        ast     integer;
        mac     integer;
        malfn   mal_func_T;
        fargs   mal_vals;
        fn_env  integer;
    BEGIN
        ast := orig_ast;
        WHILE is_macro_call(ast, env) LOOP
            mac := env_pkg.env_get(M, E, env, types.nth(M, ast, 0));
            fargs := TREAT(M(types.slice(M, ast, 1)) as mal_seq_T).val_seq;
            if M(mac).type_id = 12 THEN
                malfn := TREAT(M(mac) AS mal_func_T);
                fn_env := env_pkg.env_new(M, E, malfn.env,
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
        i         integer;
        old_seq   mal_vals;
        new_seq   mal_vals;
        new_hm    integer;
        old_midx  integer;
        new_midx  integer;
        k         varchar2(256);
    BEGIN
        IF M(ast).type_id = 7 THEN
            RETURN env_pkg.env_get(M, E, env, ast);
        ELSIF M(ast).type_id IN (8,9) THEN
            old_seq := TREAT(M(ast) AS mal_seq_T).val_seq;
            new_seq := mal_vals();
            new_seq.EXTEND(old_seq.COUNT);
            FOR i IN 1..old_seq.COUNT LOOP
                new_seq(i) := EVAL(old_seq(i), env);
            END LOOP;
            RETURN types.seq(M, M(ast).type_id, new_seq);
        ELSIF M(ast).type_id IN (10) THEN
            new_hm := types.hash_map(M, H, mal_vals());
            old_midx := TREAT(M(ast) AS mal_map_T).map_idx;
            new_midx := TREAT(M(new_hm) AS mal_map_T).map_idx;

            k := H(old_midx).FIRST();
            WHILE k IS NOT NULL LOOP
                H(new_midx)(k) := EVAL(H(old_midx)(k), env);
                k := H(old_midx).NEXT(k);
            END LOOP;
            RETURN new_hm;
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
        seq      mal_vals;
        let_env  integer;
        try_env  integer;
        i        integer;
        f        integer;
        cond     integer;
        malfn    mal_func_T;
        args     mal_vals;
    BEGIN
      WHILE TRUE LOOP
        -- io.writeline('EVAL: ' || printer.pr_str(M, H, ast));
        IF M(ast).type_id <> 8 THEN
            RETURN eval_ast(ast, env);
        END IF;

        -- apply
        ast := macroexpand(ast, env);
        IF M(ast).type_id <> 8 THEN
            RETURN eval_ast(ast, env);
        END IF;
        IF types.count(M, ast) = 0 THEN
            RETURN ast; -- empty list just returned
        END IF;

        -- apply
        a0 := types.first(M, ast);
        if M(a0).type_id = 7 THEN -- symbol
            a0sym := TREAT(M(a0) AS mal_str_T).val_str;
        ELSE
            a0sym := '__<*fn*>__';
        END IF;

        CASE
        WHEN a0sym = 'def!' THEN
            RETURN env_pkg.env_set(M, E, env,
                types.nth(M, ast, 1), EVAL(types.nth(M, ast, 2), env));
        WHEN a0sym = 'let*' THEN
            let_env := env_pkg.env_new(M, E, env);
            seq := TREAT(M(types.nth(M, ast, 1)) AS mal_seq_T).val_seq;
            i := 1;
            WHILE i <= seq.COUNT LOOP
                x := env_pkg.env_set(M, E, let_env,
                    seq(i), EVAL(seq(i+1), let_env));
                i := i + 2;
            END LOOP;
            env := let_env;
            ast := types.nth(M, ast, 2); -- TCO
        WHEN a0sym = 'quote' THEN
            RETURN types.nth(M, ast, 1);
        WHEN a0sym = 'quasiquoteexpand' THEN
            RETURN quasiquote(types.nth(M, ast, 1));
        WHEN a0sym = 'quasiquote' THEN
            RETURN EVAL(quasiquote(types.nth(M, ast, 1)), env);
        WHEN a0sym = 'defmacro!' THEN
            x := EVAL(types.nth(M, ast, 2), env);
            malfn := TREAT(M(x) as mal_func_T);
            malfn.is_macro := 1;
            M(x) := malfn;
            RETURN env_pkg.env_set(M, E, env,
                types.nth(M, ast, 1), x);
        WHEN a0sym = 'macroexpand' THEN
            RETURN macroexpand(types.nth(M, ast, 1), env);
        WHEN a0sym = 'try*' THEN
            DECLARE
                exc     integer;
                a2      integer := -1;
                a20     integer := -1;
                a20sym  varchar2(100);
            BEGIN
                RETURN EVAL(types.nth(M, ast, 1), env);

            EXCEPTION WHEN OTHERS THEN
                IF types.count(M, ast) > 2 THEN
                    a2 := types.nth(M, ast, 2);
                    IF M(a2).type_id = 8 THEN
                        a20 := types.nth(M, a2, 0);
                        IF M(a20).type_id = 7 THEN
                            a20sym := TREAT(M(a20) AS mal_str_T).val_str;
                        END IF;
                    END IF;
                END IF;
                IF a20sym = 'catch*' THEN
                    IF SQLCODE <> -20000 THEN
                        IF SQLCODE < -20000 AND SQLCODE > -20100 THEN
                            exc := types.string(M,
                                REGEXP_REPLACE(SQLERRM,
                                    '^ORA-200[0-9][0-9]: '));
                        ELSE
                            exc := types.string(M, SQLERRM);
                        END IF;
                    ELSE  -- mal throw
                        exc := err_val;
                        err_val := NULL;
                    END IF;
                    try_env := env_pkg.env_new(M, E, env,
                        types.list(M, types.nth(M, a2, 1)),
                        mal_vals(exc));
                    RETURN EVAL(types.nth(M, a2, 2), try_env);
                END IF;
                RAISE; -- not handled, re-raise the exception
            END;
        WHEN a0sym = 'do' THEN
            x := types.slice(M, ast, 1, types.count(M, ast)-2);
            x := eval_ast(x, env);
            ast := types.nth(M, ast, types.count(M, ast)-1);  -- TCO
        WHEN a0sym = 'if' THEN
            cond := EVAL(types.nth(M, ast, 1), env);
            IF cond = 1 OR cond = 2 THEN  -- nil or false
                IF types.count(M, ast) > 3 THEN
                    ast := types.nth(M, ast, 3);  -- TCO
                ELSE
                    RETURN 1;  -- nil
                END IF;
            ELSE
                ast := types.nth(M, ast, 2);  -- TCO
            END IF;
        WHEN a0sym = 'fn*' THEN
            RETURN types.malfunc(M, types.nth(M, ast, 2),
                                    types.nth(M, ast, 1),
                                    env);
        ELSE
            el := eval_ast(ast, env);
            f := types.first(M, el);
            args := TREAT(M(types.slice(M, el, 1)) AS mal_seq_T).val_seq;
            IF M(f).type_id = 12 THEN
                malfn := TREAT(M(f) AS mal_func_T);
                env := env_pkg.env_new(M, E, malfn.env,
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
    FUNCTION do_builtin(fn integer, args mal_vals) RETURN integer IS
        fname   varchar2(100);
        val     integer;
        f       integer;
        malfn   mal_func_T;
        fargs   mal_vals;
        fn_env  integer;
        i       integer;
        tseq    mal_vals;
    BEGIN
        fname := TREAT(M(fn) AS mal_str_T).val_str;
        CASE
        WHEN fname = 'do_eval' THEN
            RETURN EVAL(args(1), repl_env);
        WHEN fname = 'swap!' THEN
            val := TREAT(M(args(1)) AS mal_atom_T).val;
            f := args(2);
            -- slice one extra at the beginning that will be changed
            -- to the value of the atom
            fargs := TREAT(M(types.slice(M, args, 1)) AS mal_seq_T).val_seq;
            fargs(1) := val;
            IF M(f).type_id = 12 THEN
                malfn := TREAT(M(f) AS mal_func_T);
                fn_env := env_pkg.env_new(M, E, malfn.env,
                                          malfn.params, fargs);
                val := EVAL(malfn.ast, fn_env);
            ELSE
                val := do_builtin(f, fargs);
            END IF;
            RETURN types.atom_reset(M, args(1), val);
        WHEN fname = 'apply' THEN
            f := args(1);
            fargs := mal_vals();
            tseq := TREAT(M(args(args.COUNT())) AS mal_seq_T).val_seq;
            fargs.EXTEND(args.COUNT()-2 + tseq.COUNT());
            FOR i IN 1..args.COUNT()-2 LOOP
                fargs(i) := args(i+1);
            END LOOP;
            FOR i IN 1..tseq.COUNT() LOOP
                fargs(args.COUNT()-2 + i) := tseq(i);
            END LOOP;
            IF M(f).type_id = 12 THEN
                malfn := TREAT(M(f) AS mal_func_T);
                fn_env := env_pkg.env_new(M, E, malfn.env,
                                          malfn.params, fargs);
                val := EVAL(malfn.ast, fn_env);
            ELSE
                val := do_builtin(f, fargs);
            END IF;
            RETURN val;
        WHEN fname = 'map' THEN
            f := args(1);
            fargs := TREAT(M(args(2)) AS mal_seq_T).val_seq;
            tseq := mal_vals();
            tseq.EXTEND(fargs.COUNT());
            IF M(f).type_id = 12 THEN
                malfn := TREAT(M(f) AS mal_func_T);
                FOR i IN 1..fargs.COUNT() LOOP
                    fn_env := env_pkg.env_new(M, E, malfn.env,
                        malfn.params,
                        mal_vals(fargs(i)));
                    tseq(i) := EVAL(malfn.ast, fn_env);
                END LOOP;
            ELSE
                FOR i IN 1..fargs.COUNT() LOOP
                    tseq(i) := do_builtin(f,
                        mal_vals(fargs(i)));
                END LOOP;
            END IF;
            RETURN types.seq(M, 8, tseq);
        WHEN fname = 'throw' THEN
            err_val := args(1);
            raise_application_error(-20000, 'MalException', TRUE);
        ELSE
            RETURN core.do_core_func(M, H, fn, args);
        END CASE;
    END;


    -- print
    FUNCTION PRINT(exp integer) RETURN varchar IS
    BEGIN
        RETURN printer.pr_str(M, H, exp);
    END;

    -- repl
    FUNCTION REP(line varchar) RETURN varchar IS
    BEGIN
        RETURN PRINT(EVAL(READ(line), repl_env));
    END;

BEGIN
    -- initialize memory pools
    M := types.mem_new();
    H := types.map_entry_table();
    E := env_pkg.env_entry_table();

    repl_env := env_pkg.env_new(M, E, NULL);

    argv := TREAT(M(reader.read_str(M, H, args)) AS mal_seq_T).val_seq;

    -- core.EXT: defined using PL/SQL
    core_ns := core.get_core_ns();
    FOR cidx IN 1..core_ns.COUNT LOOP
        x := env_pkg.env_set(M, E, repl_env,
            types.symbol(M, core_ns(cidx)),
            types.func(M, core_ns(cidx)));
    END LOOP;
    x := env_pkg.env_set(M, E, repl_env,
        types.symbol(M, 'eval'),
        types.func(M, 'do_eval'));
    x := env_pkg.env_set(M, E, repl_env,
        types.symbol(M, '*ARGV*'),
        types.slice(M, argv, 1));

    -- core.mal: defined using the language itself
    line := REP('(def! *host-language* "PL/SQL")');
    line := REP('(def! not (fn* (a) (if a false true)))');
    line := REP('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))');
    line := REP('(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list ''if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons ''cond (rest (rest xs)))))))');

    IF argv.COUNT() > 0 THEN
        BEGIN
            line := REP('(load-file "' ||
                    TREAT(M(argv(1)) AS mal_str_T).val_str ||
                    '")');
            io.close(1);  -- close output stream
            RETURN 0;
        EXCEPTION WHEN OTHERS THEN
            io.writeline('Error: ' || SQLERRM);
            io.writeline(dbms_utility.format_error_backtrace);
            io.close(1);  -- close output stream
            RAISE;
        END;
    END IF;

    line := REP('(println (str "Mal [" *host-language* "]"))');
    WHILE true LOOP
        BEGIN
            line := io.readline('user> ', 0);
            IF line = EMPTY_CLOB() THEN CONTINUE; END IF;
            IF line IS NOT NULL THEN
                io.writeline(REP(line));
            END IF;

            EXCEPTION WHEN OTHERS THEN
                IF SQLCODE = -20001 THEN  -- io read stream closed
                    io.close(1);  -- close output stream
                    RETURN 0;
                END IF;
                IF SQLCODE <> -20000 THEN
                    io.writeline('Error: ' || SQLERRM);
                ELSE
                    io.writeline('Error: ' || printer.pr_str(M, H, err_val));
                END IF;
                io.writeline(dbms_utility.format_error_backtrace);
        END;
    END LOOP;
END;

END mal;
/
show errors;

quit;
