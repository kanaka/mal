\i init.sql
\i io.sql

-- ---------------------------------------------------------
-- step0_repl.sql.in

-- read
CREATE FUNCTION READ(line varchar)
RETURNS varchar AS $$
BEGIN
    RETURN line;
END; $$ LANGUAGE plpgsql;

-- eval
CREATE FUNCTION EVAL(ast varchar, env varchar)
RETURNS varchar AS $$
BEGIN
    RETURN ast;
END; $$ LANGUAGE plpgsql;

-- print
CREATE FUNCTION PRINT(exp varchar)
RETURNS varchar AS $$
BEGIN
    RETURN exp;
END; $$ LANGUAGE plpgsql;


-- repl

-- stub to support wrap.sh
CREATE FUNCTION env_vset(env integer, name varchar, val varchar)
RETURNS void AS $$
BEGIN
END; $$ LANGUAGE plpgsql;


CREATE FUNCTION REP(line varchar)
RETURNS varchar AS $$
BEGIN
    RETURN PRINT(EVAL(READ(line), ''));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION MAIN_LOOP(pwd varchar)
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
