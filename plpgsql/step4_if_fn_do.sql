\i init.sql
\i io.sql
\i types.sql
\i reader.sql
\i printer.sql
\i env.sql
\i core.sql

-- ---------------------------------------------------------
-- step1_read_print.sql

-- read
CREATE FUNCTION READ(line varchar)
RETURNS integer AS $$
BEGIN
    RETURN read_str(line);
END; $$ LANGUAGE plpgsql;

-- eval
CREATE FUNCTION eval_ast(ast integer, env integer)
RETURNS integer AS $$
DECLARE
    type           integer;
    symkey         varchar;
    vid            integer;
    k              varchar;
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
    WHEN type IN (8, 9, 10) THEN
    BEGIN
        src_coll_id := (SELECT collection_id FROM value WHERE value_id = ast);
        -- Create new value entry pointing to new collection
        dst_coll_id := COALESCE((SELECT Max(collection_id) FROM value)+1,0);
        INSERT INTO value (type_id, collection_id)
            VALUES (type, dst_coll_id)
            RETURNING value_id INTO result;
        FOR vid, k, i IN (SELECT value_id, key_string, idx FROM collection
                       WHERE collection_id = src_coll_id)
        LOOP
            -- Evaluate each entry
            e := EVAL(vid, env);
            INSERT INTO collection (collection_id, key_string, idx, value_id)
                VALUES (dst_coll_id, k, i, e);
        END LOOP;
    END;
    ELSE
        result := ast;
    END CASE;

    RETURN result;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION EVAL(ast integer, env integer)
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
    fn       integer;
    fname    varchar;
    args     integer[];
    cond     integer;
    fast     integer;
    fparams  integer;
    fenv     integer;
    result   integer;
BEGIN
    -- RAISE NOTICE 'EVAL: % [%]', pr_str(ast), ast;
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
    WHEN a0sym = 'do' THEN
    BEGIN
        el := eval_ast(_rest(ast), env);
        RETURN _nth(el, _count(el)-1);
    END;
    WHEN a0sym = 'if' THEN
    BEGIN
        cond := EVAL(_nth(ast, 1), env);
        SELECT type_id INTO type FROM value WHERE value_id = cond;
        IF type = 0 OR type = 1 THEN -- nil or false
            IF _count(ast) > 3 THEN
                RETURN EVAL(_nth(ast, 3), env);
            ELSE
                RETURN 0; -- nil
            END IF;
        ELSE
            RETURN EVAL(_nth(ast, 2), env);
        END IF;
    END;
    WHEN a0sym = 'fn*' THEN
    BEGIN
        RETURN _function(_nth(ast, 2), _nth(ast, 1), env);
    END;
    ELSE
    BEGIN
        el := eval_ast(ast, env);
        SELECT type_id, collection_id, function_name
            INTO type, fn, fname
            FROM value WHERE value_id = _first(el);
        args := _restArray(el);
        IF type = 11 THEN
            EXECUTE format('SELECT %s($1);', fname)
                INTO result USING args;
            RETURN result;
        ELSIF type = 12 THEN
            SELECT value_id, params_id, env_id
                INTO fast, fparams, fenv
                FROM collection
                WHERE collection_id = fn;
            RETURN EVAL(fast, env_new_bindings(fenv, fparams, args));
        ELSE
            RAISE EXCEPTION 'Invalid function call';
        END IF;
    END;
    END CASE;
END; $$ LANGUAGE plpgsql;

-- print
CREATE FUNCTION PRINT(exp integer) RETURNS varchar AS $$
BEGIN
    RETURN pr_str(exp);
END; $$ LANGUAGE plpgsql;


-- repl

-- repl_env is environment 0

CREATE FUNCTION REP(line varchar)
RETURNS varchar AS $$
BEGIN
    RETURN PRINT(EVAL(READ(line), 0));
END; $$ LANGUAGE plpgsql;

-- core.sql: defined using SQL (in core.sql)
-- repl_env is created and populated with core functions in by core.sql

-- core.mal: defined using the language itself
SELECT REP('(def! not (fn* (a) (if a false true)))') \g '/dev/null'

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
