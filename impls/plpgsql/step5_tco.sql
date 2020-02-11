-- ---------------------------------------------------------
-- step5_tco.sql

\i init.sql
\i io.sql
\i types.sql
\i reader.sql
\i printer.sql
\i envs.sql
\i core.sql

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
    fn       integer;
    fname    varchar;
    args     integer[];
    cond     integer;
    fast     integer;
    fparams  integer;
    fenv     integer;
    result   integer;
BEGIN
  LOOP
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
        env := let_env;
        ast := types._nth(ast, 2);
        CONTINUE; -- TCO
    END;
    WHEN a0sym = 'do' THEN
    BEGIN
        PERFORM mal.eval_ast(types._slice(ast, 1, types._count(ast)-1), env);
        ast := types._nth(ast, types._count(ast)-1);
        CONTINUE; -- TCO
    END;
    WHEN a0sym = 'if' THEN
    BEGIN
        cond := mal.EVAL(types._nth(ast, 1), env);
        SELECT type_id INTO type FROM types.value WHERE value_id = cond;
        IF type = 0 OR type = 1 THEN -- nil or false
            IF types._count(ast) > 3 THEN
                ast := types._nth(ast, 3);
                CONTINUE; -- TCO
            ELSE
                RETURN 0; -- nil
            END IF;
        ELSE
            ast := types._nth(ast, 2);
            CONTINUE; -- TCO
        END IF;
    END;
    WHEN a0sym = 'fn*' THEN
    BEGIN
        RETURN types._malfunc(types._nth(ast, 2), types._nth(ast, 1), env);
    END;
    ELSE
    BEGIN
        el := mal.eval_ast(ast, env);
        SELECT type_id, val_string, ast_id, params_id, env_id
            INTO type, fname, fast, fparams, fenv
            FROM types.value WHERE value_id = types._first(el);
        args := types._restArray(el);
        IF type = 11 THEN
            EXECUTE format('SELECT %s($1);', fname)
                INTO result USING args;
            RETURN result;
        ELSIF type = 12 THEN
            env := envs.new(fenv, fparams, args);
            ast := fast;
            CONTINUE; -- TCO
        ELSE
            RAISE EXCEPTION 'Invalid function call';
        END IF;
    END;
    END CASE;
  END LOOP;
END; $$ LANGUAGE plpgsql;

-- print
CREATE FUNCTION mal.PRINT(exp integer) RETURNS varchar AS $$
BEGIN
    RETURN printer.pr_str(exp);
END; $$ LANGUAGE plpgsql;


-- repl

-- repl_env is environment 0

CREATE FUNCTION mal.REP(line varchar) RETURNS varchar AS $$
BEGIN
    RETURN mal.PRINT(mal.EVAL(mal.READ(line), 0));
END; $$ LANGUAGE plpgsql;

-- core.sql: defined using SQL (in core.sql)
-- repl_env is created and populated with core functions in by core.sql

-- core.mal: defined using the language itself
SELECT mal.REP('(def! not (fn* (a) (if a false true)))') \g '/dev/null'

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
