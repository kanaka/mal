--\i init.sql
@io.sql

CREATE OR REPLACE PACKAGE mal IS

FUNCTION MAIN(args varchar DEFAULT '()') RETURN integer;

END mal;
/

CREATE OR REPLACE PACKAGE BODY mal IS

FUNCTION MAIN(args varchar DEFAULT '()') RETURN integer IS
    line      CLOB;

    -- read
    FUNCTION READ(line varchar) RETURN varchar IS
    BEGIN
        RETURN line;
    END;

    -- eval
    FUNCTION EVAL(ast varchar, env varchar) RETURN varchar IS
    BEGIN
        RETURN ast;
    END;

    -- print
    FUNCTION PRINT(exp varchar) RETURN varchar IS
    BEGIN
        RETURN exp;
    END;

    -- repl
    FUNCTION REP(line varchar) RETURN varchar IS
    BEGIN
        RETURN PRINT(EVAL(READ(line), ''));
    END;

BEGIN
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
