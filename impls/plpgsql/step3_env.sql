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

CREATE FUNCTION mal.eval_debug(ast integer, env integer) RETURNS void AS $$
DECLARE
    val constant integer := envs.get(env, 'DEBUG-EVAL');
BEGIN
    IF val IS NOT NULL THEN
        IF (SELECT type_id FROM types.value WHERE value_id = val) NOT IN (0, 1)
        THEN
            PERFORM io.writeline(format('EVAL: %s [%s]', mal.PRINT(ast), ast));
        END IF;
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.eval_symbol(ast integer, env integer) RETURNS integer
AS $$
    DECLARE
        symkey constant varchar := types._valueToString(ast);
        result constant integer := envs.get(env, symkey);
    BEGIN
         IF result IS NULL THEN
            RAISE EXCEPTION '''%'' not found', symkey;
        END IF;
        RETURN result;
    END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION mal.eval_vector(ast integer, env integer) RETURNS integer
AS $$
    DECLARE
        seq    constant integer[] := types._valueToArray(ast);
        eseq            integer[];
        result          integer;
    BEGIN
        -- Evaluate each entry creating a new sequence
        FOR i IN 1 .. COALESCE(array_length(seq, 1), 0) LOOP
            eseq[i] := mal.EVAL(seq[i], env);
        END LOOP;
        INSERT INTO types.value (type_id, val_seq) VALUES (9, eseq)
            RETURNING value_id INTO result;
        RETURN result;
    END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION mal.eval_map(ast integer, env integer) RETURNS integer
AS $$
    DECLARE
        hash   hstore;
        ehash  hstore;
        kv     RECORD;
        e      integer;
        result integer;
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
        INSERT INTO types.value (type_id, val_hash) VALUES (10, ehash)
            RETURNING value_id INTO result;
        RETURN result;
    END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION mal.EVAL(ast integer, env integer) RETURNS integer AS $$
DECLARE
    a0       integer;
BEGIN
    PERFORM mal.eval_debug(ast, env);

    CASE type_id FROM types.value WHERE value_id = ast
    WHEN 7  THEN RETURN mal.eval_symbol(ast, env);
    WHEN 8  THEN NULL;    --  List, proceed after this case statement.
    WHEN 9  THEN RETURN mal.eval_vector(ast, env);
    WHEN 10 THEN RETURN mal.eval_map(ast, env);
    ELSE         RETURN ast;
    END CASE;

    IF types._count(ast) = 0 THEN
        RETURN ast;
    END IF;

    a0 := types._first(ast);
    IF types._symbol_Q(a0) THEN

    CASE val_string FROM types.value WHERE value_id = a0

    WHEN 'def!' THEN
        RETURN envs.set(env, types._nth(ast, 1),
                        mal.EVAL(types._nth(ast, 2), env));

    WHEN 'let*' THEN
    DECLARE
        let_env constant integer   := envs.new(env);
        binds   constant integer[] := types._valueToArray(types._nth(ast, 1));
    BEGIN
        FOR idx IN 1 .. array_length(binds, 1) BY 2 LOOP
            PERFORM envs.set(let_env, binds[idx],
                                      mal.EVAL(binds[idx+1], let_env));
        END LOOP;
        RETURN mal.EVAL(types._nth(ast, 2), let_env);
    END;
    ELSE
        NULL;
    END CASE;
    END IF;
    --  Apply phase.
    DECLARE
        fname            varchar;
        args             integer[] := ARRAY[]::integer[];
        result           integer;
        evda0   constant integer := mal.EVAL(a0, env);
    BEGIN
        SELECT val_string INTO fname FROM types.value
            WHERE value_id = evda0;
        FOR i in 1 .. types._count(ast) - 1 LOOP
            args[i] := mal.EVAL(types._nth(ast, i), env);
        END LOOP;
        EXECUTE format('SELECT %s($1);', fname)
            INTO result USING args;
        RETURN result;
    END;
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
