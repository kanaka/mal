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
        i        integer;
        f        integer;
        cond     integer;
        malfn    mal_func_T;
        args     mal_vals;
    BEGIN
      WHILE TRUE LOOP
        -- io.writeline('EVAL: ' || printer.pr_str(M, ast));
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
    line := REP('(def! not (fn* (a) (if a false true)))');
    line := REP('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))');

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
                io.writeline('Error: ' || SQLERRM);
                io.writeline(dbms_utility.format_error_backtrace);
        END;
    END LOOP;
END;

END mal;
/
show errors;

quit;
