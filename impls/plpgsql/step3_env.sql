-- ---------------------------------------------------------
-- step3_env.sql

\i init.sql
\i io.sql
\i types.sql
\i reader.sql
\i printer.sql
\i envs.sql

-- ---------------------------------------------------------

CREATE SCHEMA mal;

-- read
CREATE FUNCTION mal.READ(line varchar) RETURNS integer AS $$
BEGIN
    RETURN reader.read_str(line);
END; $$ LANGUAGE plpgsql;

-- eval
CREATE FUNCTION mal.eval_ast(ast integer, env integer) RETURNS integer AS $$
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
    SELECT type_id INTO type FROM types.value WHERE value_id = ast;
    CASE
    WHEN type = 7 THEN
    BEGIN
        result := envs.get(env, ast);
    END;
    WHEN type IN (8, 9) THEN
    BEGIN
        SELECT val_seq INTO seq FROM types.value WHERE value_id = ast;
        -- Evaluate each entry creating a new sequence
        FOR i IN 1 .. COALESCE(array_length(seq, 1), 0) LOOP
            eseq[i] := mal.EVAL(seq[i], env);
        END LOOP;
        INSERT INTO types.value (type_id, val_seq) VALUES (type, eseq)
            RETURNING value_id INTO result;
    END;
    WHEN type = 10 THEN
    BEGIN
        SELECT val_hash INTO hash FROM types.value WHERE value_id = ast;
        -- Evaluate each value for every key/value
        FOR kv IN SELECT * FROM each(hash) LOOP
            e := mal.EVAL(CAST(kv.value AS integer), env);
            IF ehash IS NULL THEN
                ehash := hstore(kv.key, CAST(e AS varchar));
            ELSE
                ehash := ehash || hstore(kv.key, CAST(e AS varchar));
            END IF;
        END LOOP;
        INSERT INTO types.value (type_id, val_hash) VALUES (type, ehash)
            RETURNING value_id INTO result;
    END;
    ELSE
        result := ast;
    END CASE;

    RETURN result;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.EVAL(ast integer, env integer) RETURNS integer AS $$
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
    -- PERFORM writeline(format('EVAL: %s [%s]', pr_str(ast), ast));
    SELECT type_id INTO type FROM types.value WHERE value_id = ast;
    IF type <> 8 THEN
        RETURN mal.eval_ast(ast, env);
    END IF;
    IF types._count(ast) = 0 THEN
        RETURN ast;
    END IF;

    a0 := types._first(ast);
    IF types._symbol_Q(a0) THEN
        a0sym := (SELECT val_string FROM types.value WHERE value_id = a0);
    ELSE
        a0sym := '__<*fn*>__';
    END IF;

    CASE
    WHEN a0sym = 'def!' THEN
    BEGIN
        RETURN envs.set(env, types._nth(ast, 1),
                        mal.EVAL(types._nth(ast, 2), env));
    END;
    WHEN a0sym = 'let*' THEN
    BEGIN
        let_env := envs.new(env);
        a1 := types._nth(ast, 1);
        binds := (SELECT val_seq FROM types.value WHERE value_id = a1);
        idx := 1;
        WHILE idx < array_length(binds, 1) LOOP
            PERFORM envs.set(let_env, binds[idx],
                                      mal.EVAL(binds[idx+1], let_env));
            idx := idx + 2;
        END LOOP;
        RETURN mal.EVAL(types._nth(ast, 2), let_env);
    END;
    ELSE
    BEGIN
        el := mal.eval_ast(ast, env);
        SELECT val_string INTO fname FROM types.value
            WHERE value_id = types._first(el);
        args := types._restArray(el);
        EXECUTE format('SELECT %s($1);', fname)
            INTO result USING args;
        RETURN result;
    END;
    END CASE;
END; $$ LANGUAGE plpgsql;

-- print
CREATE FUNCTION mal.PRINT(exp integer) RETURNS varchar AS $$
BEGIN
    RETURN printer.pr_str(exp);
END; $$ LANGUAGE plpgsql;


-- repl

CREATE FUNCTION mal.intop(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a integer; b integer; result integer;
BEGIN
    SELECT val_int INTO a FROM types.value WHERE value_id = args[1];
    SELECT val_int INTO b FROM types.value WHERE value_id = args[2];
    EXECUTE format('INSERT INTO types.value (type_id, val_int)
                    VALUES (3, $1 %s $2)
                    RETURNING value_id;', op) INTO result USING a, b;
    RETURN result;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.add(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal.intop('+', args); END; $$ LANGUAGE plpgsql;
CREATE FUNCTION mal.subtract(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal.intop('-', args); END; $$ LANGUAGE plpgsql;
CREATE FUNCTION mal.multiply(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal.intop('*', args); END; $$ LANGUAGE plpgsql;
CREATE FUNCTION mal.divide(args integer[]) RETURNS integer AS $$
BEGIN RETURN mal.intop('/', args); END; $$ LANGUAGE plpgsql;

-- repl_env is environment 0
INSERT INTO envs.env (env_id, outer_id, data)
    VALUES (0, NULL, hstore(ARRAY['+', types._function('mal.add'),
                                  '-', types._function('mal.subtract'),
                                  '*', types._function('mal.multiply'),
                                  '/', types._function('mal.divide')]));

CREATE FUNCTION mal.REP(line varchar) RETURNS varchar AS $$
BEGIN
    RETURN mal.PRINT(mal.EVAL(mal.READ(line), 0));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.MAIN(pwd varchar) RETURNS integer AS $$
DECLARE
    line      varchar;
    output    varchar;
BEGIN
    WHILE true
    LOOP
        BEGIN
            line := io.readline('user> ', 0);
            IF line IS NULL THEN
                PERFORM io.close(1);
                RETURN 0;
            END IF;
            IF line NOT IN ('', E'\n') THEN
                output := mal.REP(line);
                PERFORM io.writeline(output);
            END IF;

            EXCEPTION WHEN OTHERS THEN
                PERFORM io.writeline('Error: ' || SQLERRM);
        END;
    END LOOP;
END; $$ LANGUAGE plpgsql;
