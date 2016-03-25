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
CREATE OR REPLACE FUNCTION eval_ast(ast integer, env integer) RETURNS integer AS $$
DECLARE
    type           integer;
    symkey         varchar;
    vid            integer;
    i              integer;
    src_coll_id    integer;
    dst_coll_id    integer = NULL;
    e              integer;
    result         integer;
BEGIN
    SELECT type_id INTO type FROM value WHERE value_id = ast;
    CASE
    WHEN type = 7 THEN
    BEGIN
        symkey := _vstring(ast);
        SELECT e.value_id FROM env e INTO result
            WHERE e.env_id = env
            AND e.key = symkey;
        IF result IS NULL THEN
            RAISE EXCEPTION '''%'' not found', symkey;
        END IF;
    END;
    WHEN type = 8 OR type = 9 THEN
    BEGIN
        src_coll_id := (SELECT collection_id FROM value WHERE value_id = ast);
        FOR vid, i IN (SELECT value_id, idx FROM collection
                       WHERE collection_id = src_coll_id)
        LOOP
            e := EVAL(vid, env);
            IF dst_coll_id IS NULL THEN
                dst_coll_id := COALESCE((SELECT Max(collection_id)
                                         FROM collection)+1,0);
            END IF;
            -- Evaluated each entry
            INSERT INTO collection (collection_id, idx, value_id)
                VALUES (dst_coll_id, i, e);
        END LOOP;
        -- Create value entry pointing to new collection
        INSERT INTO value (type_id, collection_id)
            VALUES (type, dst_coll_id)
            RETURNING value_id INTO result;
    END;
    ELSE
        result := ast;
    END CASE;

    RETURN result;
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION EVAL(ast integer, env integer) RETURNS integer AS $$
DECLARE
    type    integer;
    el      integer;
    fname   varchar;
    args    integer[];
    result  integer;
BEGIN
    SELECT type_id INTO type FROM value WHERE value_id = ast;
    IF type <> 8 THEN
        RETURN eval_ast(ast, env);
    END IF;

    el := eval_ast(ast, env);
    SELECT function_name INTO fname FROM value WHERE value_id = _first(el);
    args := _restArray(el);
    -- RAISE NOTICE 'fname: %, args: %', fname, args;
    EXECUTE format('SELECT %s($1);', fname)
        INTO result USING args;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- print
CREATE OR REPLACE FUNCTION PRINT(exp integer) RETURNS varchar AS $$
BEGIN
    RETURN pr_str(exp);
END; $$ LANGUAGE plpgsql;


-- repl

-- env table
CREATE TABLE env (
    env_id    integer NOT NULL,
    key       varchar NOT NULL,
    value_id  integer NOT NULL
);

CREATE OR REPLACE FUNCTION env_vset(env integer, name varchar, val integer)
    RETURNS void AS $$
BEGIN
    INSERT INTO env (env_id, key, value_id) VALUES (env, name, val);
END; $$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION mal_intop(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a integer; b integer; result integer;
BEGIN
    SELECT val_int INTO a FROM value WHERE value_id = args[1];
    SELECT val_int INTO b FROM value WHERE value_id = args[2];
    EXECUTE format('INSERT INTO value (type_id, val_int) VALUES (3, $1 %s $2)
                    RETURNING value_id;', op) INTO result USING a, b;
    RETURN result;
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION mal_add(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal_intop('+', args); END; $$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION mal_subtract(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal_intop('-', args); END; $$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION mal_multiply(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal_intop('*', args); END; $$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION mal_divide(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal_intop('/', args); END; $$ LANGUAGE plpgsql;

INSERT INTO value (type_id, function_name) VALUES (11, 'mal_add');
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_subtract');
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_multiply');
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_divide');

-- repl_env is environment 0
SELECT env_vset(0, '+', (SELECT value_id FROM value WHERE function_name = 'mal_add'));
SELECT env_vset(0, '-', (SELECT value_id FROM value WHERE function_name = 'mal_subtract'));
SELECT env_vset(0, '*', (SELECT value_id FROM value WHERE function_name = 'mal_multiply'));
SELECT env_vset(0, '/', (SELECT value_id FROM value WHERE function_name = 'mal_divide'));


CREATE OR REPLACE FUNCTION REP(line varchar) RETURNS varchar AS $$
DECLARE
    output varchar;
BEGIN
    -- RAISE NOTICE 'line is %', line;
    -- output := 'line: ' || line;
    RETURN PRINT(EVAL(READ(line), 0));
END; $$ LANGUAGE plpgsql;
