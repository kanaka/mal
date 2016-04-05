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
    line  varchar2(4000);

    -- read
    FUNCTION READ(line varchar) RETURN mal_type IS
    BEGIN
        RETURN reader_pkg.read_str(line);
    END;

    -- eval
    FUNCTION EVAL(ast mal_type, env varchar) RETURN mal_type IS
    BEGIN
        RETURN ast;
    END;

    -- print
    FUNCTION PRINT(exp mal_type) RETURN varchar IS
    BEGIN
        RETURN printer_pkg.pr_str(exp);
    END;

    -- repl
    FUNCTION REP(line varchar) RETURN varchar IS
    BEGIN
        RETURN PRINT(EVAL(READ(line), ''));
    END;

BEGIN
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
