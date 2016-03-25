\i init.sql
\i io.sql

-- ---------------------------------------------------------
-- step0_repl.sql.in

-- read
CREATE OR REPLACE FUNCTION READ(line varchar)
RETURNS varchar AS $$
BEGIN
    RETURN line;
END; $$ LANGUAGE plpgsql;

-- eval
CREATE OR REPLACE FUNCTION EVAL(ast varchar, env varchar)
RETURNS varchar AS $$
BEGIN
    RETURN ast;
END; $$ LANGUAGE plpgsql;

-- print
CREATE OR REPLACE FUNCTION PRINT(exp varchar)
RETURNS varchar AS $$
BEGIN
    RETURN exp;
END; $$ LANGUAGE plpgsql;


-- repl

-- stub to support wrap.sh
CREATE OR REPLACE FUNCTION env_vset(env integer, name varchar, val varchar)
RETURNS void AS $$
BEGIN
END; $$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION REP(line varchar)
RETURNS varchar AS $$
BEGIN
    RETURN PRINT(EVAL(READ(line), ''));
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION MAIN_LOOP()
RETURNS integer AS $$
DECLARE
    line    varchar;
    output  varchar;
BEGIN
    WHILE true
    LOOP
        BEGIN
            line := readline('user> ', 0);
            IF line IS NULL THEN RETURN 0; END IF;
            IF line <> '' THEN
                output := REP(line);
                PERFORM writeline(output);
            END IF;

            EXCEPTION WHEN OTHERS THEN
                PERFORM writeline('Error: ' || SQLERRM);
        END;
    END LOOP;
END; $$ LANGUAGE plpgsql;
