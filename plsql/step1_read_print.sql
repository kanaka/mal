@io.sql
@types.sql
@reader.sql
@printer.sql

CREATE OR REPLACE PACKAGE mal IS

FUNCTION MAIN(args varchar DEFAULT '()') RETURN integer;

END mal;
/

CREATE OR REPLACE PACKAGE BODY mal IS

FUNCTION MAIN(args varchar DEFAULT '()') RETURN integer IS
    M         types.mal_table;                 -- general mal value memory pool
    H         types.map_entry_table;    -- hashmap memory pool
    line      CLOB;

    -- read
    FUNCTION READ(line varchar) RETURN integer IS
    BEGIN
        RETURN reader.read_str(M, H, line);
    END;

    -- eval
    FUNCTION EVAL(ast integer, env varchar) RETURN integer IS
    BEGIN
        RETURN ast;
    END;

    -- print
    FUNCTION PRINT(exp integer) RETURN varchar IS
    BEGIN
        RETURN printer.pr_str(M, H, exp);
    END;

    -- repl
    FUNCTION REP(line varchar) RETURN varchar IS
    BEGIN
        RETURN PRINT(EVAL(READ(line), ''));
    END;

BEGIN
    -- initialize memory pools
    M := types.mem_new();
    H := types.map_entry_table();

    WHILE true LOOP
        BEGIN
            line := io.readline('user> ', 0);
            IF line = EMPTY_CLOB() THEN CONTINUE; END IF;
            IF line IS NOT NULL THEN
                io.writeline(REP(line));
            END IF;

            EXCEPTION WHEN OTHERS THEN
                IF SQLCODE = -20001 THEN  -- io read stream closed
                    io.writeline('closing stream 1');
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
