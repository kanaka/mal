\i init.sql

-- ---------------------------------------------------------
-- step0_repl.sql.in

-- read
CREATE OR REPLACE FUNCTION READ(line varchar) RETURNS varchar
AS $$
BEGIN
    RETURN line;
END; $$ LANGUAGE plpgsql;

-- eval
CREATE OR REPLACE FUNCTION EVAL(ast varchar, env varchar) RETURNS varchar AS $$
BEGIN
    RETURN ast;
END; $$ LANGUAGE plpgsql;

-- print
CREATE OR REPLACE FUNCTION PRINT(exp varchar) RETURNS varchar AS $$
BEGIN
    RETURN exp;
END; $$ LANGUAGE plpgsql;


-- repl

-- stub to support wrap.sh
CREATE OR REPLACE FUNCTION env_vset(env integer, name varchar, val varchar)
    RETURNS void AS $$
BEGIN END; $$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION REP(line varchar) RETURNS varchar AS $$
DECLARE
    output varchar;
BEGIN
    -- RAISE NOTICE 'line is %', line;
    -- output := 'line: ' || line;
    RETURN line;
END; $$ LANGUAGE plpgsql;
