@io.sql
@types.sql
@reader.sql
@printer.sql

-- ---------------------------------------------------------
-- step1_read_print.sql

-- read
CREATE OR REPLACE FUNCTION READ(line varchar)
RETURN mal_type IS
BEGIN
    RETURN reader_pkg.read_str(line);
END;
/

-- eval
CREATE OR REPLACE FUNCTION EVAL(ast mal_type, env varchar)
RETURN mal_type IS
BEGIN
    RETURN ast;
END;
/

-- print
CREATE OR REPLACE FUNCTION PRINT(exp mal_type)
RETURN varchar IS
BEGIN
    RETURN printer_pkg.pr_str(exp);
END;
/


-- repl

-- stub to support wrap.sh
CREATE OR REPLACE PROCEDURE env_vset(env integer, name varchar, val varchar)
IS
BEGIN
    RETURN;
END;
/


CREATE OR REPLACE FUNCTION REP(line varchar)
RETURN varchar IS
BEGIN
    RETURN PRINT(EVAL(READ(line), ''));
END;
/

CREATE OR REPLACE FUNCTION MAIN_LOOP(pwd varchar)
RETURN integer IS
    line    varchar2(4000);
    output  varchar2(4000);
BEGIN
    WHILE true
    LOOP
        BEGIN
            line := stream_readline('user> ', 0);
            -- stream_writeline('line: [' || line || ']', 1);
            IF line IS NULL THEN RETURN 0; END IF;
            IF line IS NOT NULL THEN
                output := REP(line);
                stream_writeline(output);
            END IF;

            EXCEPTION WHEN OTHERS THEN
                IF SQLCODE = -20000 THEN
                    RETURN 0;
                END IF;
                stream_writeline('Error: ' || SQLERRM);
        END;
    END LOOP;
END;
/

quit;
