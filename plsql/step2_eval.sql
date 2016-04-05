@io.sql
@types.sql
@reader.sql
@printer.sql

CREATE OR REPLACE PACKAGE mal_pkg IS

FUNCTION MAIN(pwd varchar) RETURN integer;

END mal_pkg;
/

CREATE OR REPLACE PACKAGE BODY mal_pkg IS

FUNCTION MAIN(pwd varchar) RETURN integer IS
    TYPE env_type IS TABLE OF mal_type INDEX BY varchar2(100);
    repl_env  env_type;

    line  varchar2(4000);

    -- read
    FUNCTION READ(line varchar) RETURN mal_type IS
    BEGIN
        RETURN reader_pkg.read_str(line);
    END;

    -- eval

    -- forward declarations
    FUNCTION EVAL(ast mal_type, env env_type) RETURN mal_type;
    FUNCTION do_core_func(fn mal_type, args mal_seq_items_type) RETURN mal_type;

    FUNCTION eval_ast(ast mal_type, env env_type) RETURN mal_type IS
        i        integer;
        old_seq  mal_seq_items_type;
        new_seq  mal_seq_items_type;
        f        mal_type;
    BEGIN
        IF ast.type_id = 7 THEN
            RETURN env(TREAT(ast AS mal_str_type).val_str);
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

    FUNCTION EVAL(ast mal_type, env env_type) RETURN mal_type IS
        el    mal_type;
        f     mal_type;
        args  mal_seq_type;
    BEGIN
        IF ast.type_id <> 8 THEN
            RETURN eval_ast(ast, env);
        END IF;

        -- apply
        el := eval_ast(ast, env);
        f := types_pkg.first(el);
        args := TREAT(types_pkg.slice(el, 1) AS mal_seq_type);
        RETURN do_core_func(f, args.val_seq);
    END;

    -- print
    FUNCTION PRINT(exp mal_type) RETURN varchar IS
    BEGIN
        RETURN printer_pkg.pr_str(exp);
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
    repl_env('+') := mal_str_type(11, '+');
    repl_env('-') := mal_str_type(11, '-');
    repl_env('*') := mal_str_type(11, '*');
    repl_env('/') := mal_str_type(11, '/');

    WHILE true LOOP
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
