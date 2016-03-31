\i init.sql
\i io.sql
\i types.sql
\i reader.sql
\i printer.sql
\i env.sql

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
    seq            integer[];
    eseq           integer[];
    hash           hstore;
    ehash          hstore;
    kv             RECORD;
    e              integer;
    result         integer;
BEGIN
    SELECT type_id INTO type FROM value WHERE value_id = ast;
    CASE
    WHEN type = 7 THEN
    BEGIN
        result := env_get(env, ast);
    END;
    WHEN type IN (8, 9) THEN
    BEGIN
        SELECT val_seq INTO seq FROM value WHERE value_id = ast;
        -- Evaluate each entry creating a new sequence
        FOR i IN 1 .. COALESCE(array_length(seq, 1), 0) LOOP
            eseq[i] := EVAL(seq[i], env);
        END LOOP;
        INSERT INTO value (type_id, val_seq) VALUES (type, eseq)
            RETURNING value_id INTO result;
    END;
    WHEN type = 10 THEN
    BEGIN
        SELECT val_hash INTO hash FROM value WHERE value_id = ast;
        -- Evaluate each value for every key/value
        FOR kv IN SELECT * FROM each(hash) LOOP
            e := EVAL(CAST(kv.value AS integer), env);
            IF ehash IS NULL THEN
                ehash := hstore(kv.key, CAST(e AS varchar));
            ELSE
                ehash := ehash || hstore(kv.key, CAST(e AS varchar));
            END IF;
        END LOOP;
        INSERT INTO value (type_id, val_hash) VALUES (type, ehash)
            RETURNING value_id INTO result;
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
    idx      integer;
    binds    integer[];
    el       integer;
    fname    varchar;
    args     integer[];
    result   integer;
BEGIN
    --PERFORM writeline(format('EVAL: %s [%s]', pr_str(ast), ast));
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

    CASE
    WHEN a0sym = 'def!' THEN
    BEGIN
        RETURN env_set(env, _nth(ast, 1), EVAL(_nth(ast, 2), env));
    END;
    WHEN a0sym = 'let*' THEN
    BEGIN
        let_env := env_new(env);
        a1 := _nth(ast, 1);
        binds := (SELECT val_seq FROM value WHERE value_id = a1);
        idx := 1;
        WHILE idx < array_length(binds, 1) LOOP
            PERFORM env_set(let_env, binds[idx], EVAL(binds[idx+1], let_env));
            idx := idx + 2;
        END LOOP;
        RETURN EVAL(_nth(ast, 2), let_env);
    END;
    ELSE
    BEGIN
        el := eval_ast(ast, env);
        SELECT val_string INTO fname FROM value WHERE value_id = _first(el);
        args := _restArray(el);
        EXECUTE format('SELECT %s($1);', fname)
            INTO result USING args;
        RETURN result;
    END;
    END CASE;
END; $$ LANGUAGE plpgsql;

-- print
CREATE FUNCTION PRINT(exp integer)
RETURNS varchar AS $$
BEGIN
    RETURN pr_str(exp);
END; $$ LANGUAGE plpgsql;


-- repl

CREATE FUNCTION mal_intop(op varchar, args integer[])
RETURNS integer AS $$
DECLARE a integer; b integer; result integer;
BEGIN
    SELECT val_int INTO a FROM value WHERE value_id = args[1];
    SELECT val_int INTO b FROM value WHERE value_id = args[2];
    EXECUTE format('INSERT INTO value (type_id, val_int) VALUES (3, $1 %s $2)
                    RETURNING value_id;', op) INTO result USING a, b;
    RETURN result;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_add(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal_intop('+', args); END; $$ LANGUAGE plpgsql;
CREATE FUNCTION mal_subtract(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal_intop('-', args); END; $$ LANGUAGE plpgsql;
CREATE FUNCTION mal_multiply(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal_intop('*', args); END; $$ LANGUAGE plpgsql;
CREATE FUNCTION mal_divide(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal_intop('/', args); END; $$ LANGUAGE plpgsql;

-- repl_env is environment 0
INSERT INTO env (env_id, outer_id, data)
    VALUES (0, NULL, hstore(ARRAY['+', _function('mal_add'),
                                  '-', _function('mal_subtract'),
                                  '*', _function('mal_multiply'),
                                  '/', _function('mal_divide')]));

CREATE FUNCTION REP(line varchar)
RETURNS varchar AS $$
BEGIN
    RETURN PRINT(EVAL(READ(line), 0));
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
