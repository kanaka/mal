-- ---------------------------------------------------------
-- step2_eval.sql

\i init.sql
\i io.sql
\i types.sql
\i reader.sql
\i printer.sql

-- ---------------------------------------------------------

CREATE SCHEMA mal;

-- read
CREATE FUNCTION mal.READ(line varchar) RETURNS integer AS $$
BEGIN
    RETURN reader.read_str(line);
END; $$ LANGUAGE plpgsql;

-- eval

CREATE FUNCTION mal.eval_symbol(ast integer, env hstore) RETURNS integer
AS $$
    DECLARE
        symkey constant varchar := types._valueToString(ast);
    BEGIN
        IF env ? symkey THEN
            RETURN env -> symkey;
        ELSE
            RAISE EXCEPTION '''%'' not found', symkey;
        END IF;
    END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION mal.eval_vector(ast integer, env hstore) RETURNS integer
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

CREATE FUNCTION mal.eval_map(ast integer, env hstore) RETURNS integer
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

CREATE FUNCTION mal.EVAL(ast integer, env hstore) RETURNS integer AS $$
DECLARE
    a0       integer;
    fname    varchar;
    args     integer[] := ARRAY[]::integer[];
    evda0    integer;
    result   integer;
BEGIN
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
    evda0  := mal.EVAL(a0, env);
    SELECT val_string INTO fname FROM types.value
        WHERE value_id = evda0;
    FOR i in 1 .. types._count(ast) - 1 LOOP
        args[i] := mal.EVAL(types._nth(ast, i), env);
    END LOOP;
    EXECUTE format('SELECT %s($1);', fname) INTO result USING args;
    RETURN result;
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


CREATE FUNCTION mal.REP(env hstore, line varchar) RETURNS varchar AS $$
BEGIN
    RETURN mal.PRINT(mal.EVAL(mal.READ(line), env));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.MAIN(pwd varchar) RETURNS integer AS $$
DECLARE
    repl_env  hstore;
    line      varchar;
    output    varchar;
BEGIN
    repl_env := hstore(ARRAY[
        '+', types._function('mal.add'),
        '-', types._function('mal.subtract'),
        '*', types._function('mal.multiply'),
        '/', types._function('mal.divide')]);
    WHILE true LOOP
        BEGIN
            line := io.readline('user> ', 0);
            IF line IS NULL THEN
                PERFORM io.close(1);
                RETURN 0;
            END IF;
            IF line NOT IN ('', E'\n') THEN
                output := mal.REP(repl_env, line);
                PERFORM io.writeline(output);
            END IF;

            EXCEPTION WHEN OTHERS THEN
                PERFORM io.writeline('Error: ' || SQLERRM);
        END;
    END LOOP;
END; $$ LANGUAGE plpgsql;
