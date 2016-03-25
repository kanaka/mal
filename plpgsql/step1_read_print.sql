\i init.sql
\i types.sql
\i reader.sql
\i printer.sql

-- ---------------------------------------------------------
-- step1_read_print.sql

-- read
CREATE OR REPLACE FUNCTION READ(line varchar) RETURNS integer AS $$
BEGIN
    RETURN read_str(line);
END; $$ LANGUAGE plpgsql;

-- eval
CREATE OR REPLACE FUNCTION EVAL(ast integer, env varchar) RETURNS integer AS $$
BEGIN
    RETURN ast;
END; $$ LANGUAGE plpgsql;

-- print
CREATE OR REPLACE FUNCTION PRINT(exp integer) RETURNS varchar AS $$
BEGIN
    RETURN pr_str(exp);
END; $$ LANGUAGE plpgsql;


-- repl

-- stub to support wrap.sh
CREATE OR REPLACE FUNCTION env_vset(env integer, name varchar, val integer)
    RETURNS void AS $$
BEGIN END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION REP(line varchar) RETURNS varchar AS $$
DECLARE
    output varchar;
BEGIN
    -- RAISE NOTICE 'line is %', line;
    -- output := 'line: ' || line;
    RETURN PRINT(EVAL(READ(line), ''));
END; $$ LANGUAGE plpgsql;

