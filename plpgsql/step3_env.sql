\i init.sql
\i io.sql
\i types.sql
\i reader.sql
\i printer.sql
\i env.sql

-- ---------------------------------------------------------
-- step1_read_print.sql

-- read
CREATE OR REPLACE FUNCTION READ(line varchar)
RETURNS integer AS $$
BEGIN
    RETURN read_str(line);
END; $$ LANGUAGE plpgsql;

-- eval
CREATE OR REPLACE FUNCTION eval_ast(ast integer, env integer)
RETURNS integer AS $$
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
        result := env_get(env, ast);
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

CREATE OR REPLACE FUNCTION EVAL(ast integer, env integer)
RETURNS integer AS $$
DECLARE
    type     integer;
    a0       integer;
    a0sym    varchar;
    a1       integer;
    let_env  integer;
    binds    integer[];
    exprs    integer[];
    el       integer;
    fname    varchar;
    args     integer[];
    result   integer;
BEGIN
    SELECT type_id INTO type FROM value WHERE value_id = ast;
    IF type <> 8 THEN
        RETURN eval_ast(ast, env);
    END IF;

    a0 := _first(ast);
    IF _symbol_Q(a0) THEN
        a0sym := (SELECT val_string FROM value WHERE value_id = a0);
    ELSE
        a0sym := '__<*fn*>__';
    END IF;

    --RAISE NOTICE 'ast: %, a0sym: %', ast, a0sym;
    CASE
    WHEN a0sym = 'def!' THEN
    BEGIN
        RETURN env_set(env, _nth(ast, 1), EVAL(_nth(ast, 2), env));
    END;
    WHEN a0sym = 'let*' THEN
    BEGIN
        let_env := env_new(env);
        a1 := _nth(ast, 1);
        binds := ARRAY(SELECT collection.value_id FROM collection INNER JOIN value
                       ON collection.collection_id=value.collection_id
                       WHERE value.value_id = a1
                       AND (collection.idx % 2) = 0
                       ORDER BY collection.idx);
        exprs := ARRAY(SELECT collection.value_id FROM collection INNER JOIN value
                       ON collection.collection_id=value.collection_id
                       WHERE value.value_id = a1
                       AND (collection.idx % 2) = 1
                       ORDER BY collection.idx);
        FOR idx IN array_lower(binds, 1) .. array_upper(binds, 1)
        LOOP
            PERFORM env_set(let_env, binds[idx], EVAL(exprs[idx], let_env));
        END LOOP;
        --PERFORM env_print(let_env);
        RETURN EVAL(_nth(ast, 2), let_env);
    END;
    ELSE
    BEGIN
        el := eval_ast(ast, env);
        SELECT function_name INTO fname FROM value WHERE value_id = _first(el);
        args := _restArray(el);
        --RAISE NOTICE 'fname: %, args: %', fname, args;
        EXECUTE format('SELECT %s($1);', fname)
            INTO result USING args;
        RETURN result;
    END;
    END CASE;
END; $$ LANGUAGE plpgsql;

-- print
CREATE OR REPLACE FUNCTION PRINT(exp integer) RETURNS varchar AS $$
BEGIN
    RETURN pr_str(exp);
END; $$ LANGUAGE plpgsql;


-- repl

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
INSERT INTO env (env_id, outer_id) VALUES (0, NULL);

SELECT env_vset(0, '+', (SELECT value_id FROM value WHERE function_name = 'mal_add'));
SELECT env_vset(0, '-', (SELECT value_id FROM value WHERE function_name = 'mal_subtract'));
SELECT env_vset(0, '*', (SELECT value_id FROM value WHERE function_name = 'mal_multiply'));
SELECT env_vset(0, '/', (SELECT value_id FROM value WHERE function_name = 'mal_divide'));


CREATE OR REPLACE FUNCTION REP(line varchar)
RETURNS varchar AS $$
BEGIN
    RETURN PRINT(EVAL(READ(line), 0));
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
