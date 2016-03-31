\i init.sql
\i io.sql
\i types.sql
\i reader.sql
\i printer.sql

-- ---------------------------------------------------------
-- step1_read_print.sql

-- read
CREATE FUNCTION READ(line varchar)
RETURNS integer AS $$
BEGIN
    RETURN read_str(line);
END; $$ LANGUAGE plpgsql;

-- eval
CREATE FUNCTION eval_ast(ast integer, env hstore)
RETURNS integer AS $$
DECLARE
    type           integer;
    symkey         varchar;
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
        symkey := _valueToString(ast);
        IF env ? symkey THEN
            result := env -> symkey;
        ELSE
            RAISE EXCEPTION '''%'' not found', symkey;
        END IF;
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

CREATE FUNCTION EVAL(ast integer, env hstore)
RETURNS integer AS $$
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
    EXECUTE format('SELECT %s($1);', fname) INTO result USING args;
    RETURN result;
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


CREATE FUNCTION REP(env hstore, line varchar)
RETURNS varchar AS $$
BEGIN
    RETURN PRINT(EVAL(READ(line), env));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION MAIN_LOOP(pwd varchar)
RETURNS integer AS $$
DECLARE
    repl_env  hstore;
    line      varchar;
    output    varchar;
BEGIN
    repl_env := hstore(ARRAY[
        '+', _function('mal_add'),
        '-', _function('mal_subtract'),
        '*', _function('mal_multiply'),
        '/', _function('mal_divide')]);
    WHILE true LOOP
        BEGIN
            line := readline('user> ', 0);
            IF line IS NULL THEN RETURN 0; END IF;
            IF line <> '' THEN
                output := REP(repl_env, line);
                PERFORM writeline(output);
            END IF;

            EXCEPTION WHEN OTHERS THEN
                PERFORM writeline('Error: ' || SQLERRM);
        END;
    END LOOP;
END; $$ LANGUAGE plpgsql;
