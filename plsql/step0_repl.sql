--\i init.sql
@io.sql

-- ---------------------------------------------------------
-- step0_repl.sql

-- read
CREATE OR REPLACE FUNCTION READ(line varchar)
RETURN varchar IS
BEGIN
    RETURN line;
END;
/

-- eval
CREATE OR REPLACE FUNCTION EVAL(ast varchar, env varchar)
RETURN varchar IS
BEGIN
    RETURN ast;
END;
/

-- print
CREATE OR REPLACE FUNCTION PRINT(exp varchar)
RETURN varchar IS
BEGIN
    RETURN exp;
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
