@io.sql
@types.sql
@reader.sql
@printer.sql
@env.sql

-- ---------------------------------------------------------
-- step1_read_print.sql

CREATE OR REPLACE PACKAGE mal_pkg IS

FUNCTION MAIN(pwd varchar) RETURN integer;

END mal_pkg;
/

CREATE OR REPLACE PACKAGE BODY mal_pkg IS

FUNCTION MAIN(pwd varchar) RETURN integer IS
    env_mem   env_mem_type;
    repl_env  integer;
    line      varchar2(4000);
    x         mal_type;

    -- read
    FUNCTION READ(line varchar) RETURN mal_type IS
    BEGIN
        RETURN reader_pkg.read_str(line);
    END;

    -- eval

    -- forward declarations
    FUNCTION EVAL(ast mal_type, env integer) RETURN mal_type;
    FUNCTION do_core_func(fn mal_type, args mal_seq_items_type) RETURN mal_type;

    FUNCTION eval_ast(ast mal_type, env integer) RETURN mal_type IS
        i        integer;
        old_seq  mal_seq_items_type;
        new_seq  mal_seq_items_type;
        f        mal_type;
    BEGIN
        IF ast.type_id = 7 THEN
            RETURN env_pkg.env_get(env_mem, env, ast);
        ELSIF ast.type_id IN (8,9) THEN
            old_seq := TREAT(ast AS mal_seq_type).val_seq;
            new_seq := mal_seq_items_type();
            new_seq.EXTEND(old_seq.COUNT);
            FOR i IN 1..old_seq.COUNT LOOP
                new_seq(i) := EVAL(old_seq(i), env);
            END LOOP;
            RETURN mal_seq_type(ast.type_id, new_seq);
        ELSE
            RETURN ast;
        END IF;
    END;

    FUNCTION EVAL(ast mal_type, env integer) RETURN mal_type IS
        el       mal_type;
        a0       mal_type;
        a0sym    varchar2(4000);
        seq      mal_seq_items_type;
        let_env  integer;
        i        integer;
        f        mal_type;
        args     mal_seq_type;
    BEGIN
        IF ast.type_id <> 8 THEN
            RETURN eval_ast(ast, env);
        END IF;

        -- apply
        a0 := types_pkg.first(ast);
        if a0.type_id = 7 THEN -- symbol
            a0sym := TREAT(a0 AS mal_str_type).val_str;
        ELSE
            a0sym := '__<*fn*>__';
        END IF;

        CASE
        WHEN a0sym = 'def!' THEN
            RETURN env_pkg.env_set(env_mem, env,
                types_pkg.nth(ast, 1), EVAL(types_pkg.nth(ast, 2), env));
        WHEN a0sym = 'let*' THEN
            let_env := env_pkg.env_new(env_mem, env);
            seq := TREAT(types_pkg.nth(ast, 1) AS mal_seq_type).val_seq;
            i := 1;
            WHILE i <= seq.COUNT LOOP
                x := env_pkg.env_set(env_mem, let_env,
                    seq(i), EVAL(seq(i+1), let_env));
                i := i + 2;
            END LOOP;
            RETURN EVAL(types_pkg.nth(ast, 2), let_env);
        ELSE
            el := eval_ast(ast, env);
            f := types_pkg.first(el);
            args := TREAT(types_pkg.slice(el, 1) AS mal_seq_type);
            RETURN do_core_func(f, args.val_seq);
        END CASE;

    END;

    -- print
    FUNCTION PRINT(exp mal_type) RETURN varchar IS
    BEGIN
        RETURN printer_pkg.pr_str(exp);
    END;

    -- stub to support wrap.sh
    PROCEDURE env_vset(env integer, name varchar, val varchar) IS
    BEGIN
        RETURN;
    END;

    -- repl
    FUNCTION mal_add(args mal_seq_items_type) RETURN mal_type IS
    BEGIN
        RETURN mal_int_type(3, TREAT(args(1) AS mal_int_type).val_int +
                               TREAT(args(2) AS mal_int_type).val_int);
    END;

    FUNCTION mal_subtract(args mal_seq_items_type) RETURN mal_type IS
    BEGIN
        RETURN mal_int_type(3, TREAT(args(1) AS mal_int_type).val_int -
                               TREAT(args(2) AS mal_int_type).val_int);
    END;

    FUNCTION mal_multiply(args mal_seq_items_type) RETURN mal_type IS
    BEGIN
        RETURN mal_int_type(3, TREAT(args(1) AS mal_int_type).val_int *
                               TREAT(args(2) AS mal_int_type).val_int);
    END;

    FUNCTION mal_divide(args mal_seq_items_type) RETURN mal_type IS
    BEGIN
        RETURN mal_int_type(3, TREAT(args(1) AS mal_int_type).val_int /
                               TREAT(args(2) AS mal_int_type).val_int);
    END;

    FUNCTION do_core_func(fn mal_type, args mal_seq_items_type) RETURN mal_type IS
        fname  varchar(100);
    BEGIN
        IF fn.type_id <> 11 THEN
            raise_application_error(-20004,
                'Invalid function call', TRUE);
        END IF;

        fname := TREAT(fn AS mal_str_type).val_str;
        CASE
        WHEN fname = '+' THEN RETURN mal_add(args);
        WHEN fname = '-' THEN RETURN mal_subtract(args);
        WHEN fname = '*' THEN RETURN mal_multiply(args);
        WHEN fname = '/' THEN RETURN mal_divide(args);
        ELSE raise_application_error(-20004,
                'Invalid function call', TRUE);
        END CASE;
    END;

    FUNCTION REP(line varchar) RETURN varchar IS
    BEGIN
        RETURN PRINT(EVAL(READ(line), repl_env));
    END;

BEGIN
    env_mem := env_mem_type();
    repl_env := env_pkg.env_new(env_mem, NULL);
    x := env_pkg.env_set(env_mem, repl_env, types_pkg.symbol('+'),
                                            mal_str_type(11, '+'));
    x := env_pkg.env_set(env_mem, repl_env, types_pkg.symbol('-'),
                                            mal_str_type(11, '-'));
    x := env_pkg.env_set(env_mem, repl_env, types_pkg.symbol('*'),
                                            mal_str_type(11, '*'));
    x := env_pkg.env_set(env_mem, repl_env, types_pkg.symbol('/'),
                                            mal_str_type(11, '/'));

    WHILE true
    LOOP
        BEGIN
            line := stream_readline('user> ', 0);
            -- stream_writeline('line: [' || line || ']', 1);
            IF line IS NULL THEN RETURN 0; END IF;
            IF line IS NOT NULL THEN
                stream_writeline(REP(line));
            END IF;

            EXCEPTION WHEN OTHERS THEN
                IF SQLCODE = -20000 THEN
                    RETURN 0;
                END IF;
                stream_writeline('Error: ' || SQLERRM);
                stream_writeline(dbms_utility.format_error_backtrace);
        END;
    END LOOP;
END;

END mal_pkg;
/
show errors;

quit;
