-- ---------------------------------------------------------
-- step7_quote.sql

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

CREATE FUNCTION mal.qq_loop(elt integer, acc integer) RETURNS integer AS $$
DECLARE
    a0 integer;
BEGIN
    IF types._list_Q(elt) AND types._count(elt) = 2 THEN
        a0 := types._first(elt);
        IF types._symbol_Q(a0) AND a0 = types._symbolv('splice-unquote') THEN
            RETURN types._list(ARRAY[types._symbolv('concat'), types._nth(elt, 1), acc]);
        END IF;
    END IF;
    RETURN types._list(ARRAY[types._symbolv('cons'), mal.quasiquote(elt), acc]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.qq_foldr(xs integer) RETURNS integer AS $$
DECLARE
    elt integer;
    acc integer := types._list(ARRAY[]::integer[]);
BEGIN
    FOREACH elt IN ARRAY types.array_reverse(types._valueToArray(xs)) LOOP
        acc := mal.qq_loop(elt, acc);
    END LOOP;
    RETURN acc;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.quasiquote(ast integer) RETURNS integer AS $$
BEGIN
    CASE type_id FROM types.value WHERE value_id = ast
    WHEN 8 THEN                 --  list
    DECLARE
        a0   integer;
    BEGIN
        IF types._count(ast) = 2 THEN
            a0 := types._first(ast);
            IF types._symbol_Q(a0) AND a0 = types._symbolv('unquote') THEN
                RETURN types._nth(ast, 1);
            END IF;
        END IF;
        RETURN mal.qq_foldr(ast);
    END;
    WHEN 9 THEN                 --  vector
        RETURN types._list(ARRAY[types._symbolv('vec'), mal.qq_foldr(ast)]);
    WHEN 7, 10 THEN             --  symbol or map
        RETURN types._list(ARRAY[types._symbolv('quote'), ast]);
    ELSE
        RETURN ast;
    END CASE;
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
  LOOP

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
        env := let_env;
        ast := types._nth(ast, 2);
        CONTINUE; -- TCO
    END;

    WHEN 'quote' THEN
        RETURN types._nth(ast, 1);

    WHEN 'quasiquote' THEN
    BEGIN
        ast := mal.quasiquote(types._nth(ast, 1));
        CONTINUE; -- TCO
    END;

    WHEN 'do' THEN
    DECLARE
        ignored  integer;
    BEGIN
        FOR i IN 1 .. types._count(ast) - 2 LOOP
            ignored := mal.EVAL(types._nth(ast, i), env);
        END LOOP;
        ast := types._nth(ast, types._count(ast)-1);
        CONTINUE; -- TCO
    END;

    WHEN 'if' THEN
        IF (SELECT type_id FROM types.value
            WHERE value_id = mal.EVAL(types._nth(ast, 1), env))
           IN (0, 1)
        THEN  --  nil or false
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

    WHEN 'fn*' THEN
        RETURN types._malfunc(types._nth(ast, 2), types._nth(ast, 1), env);

    ELSE
        NULL;
    END CASE;
    END IF;
    --  Apply phase.
    DECLARE
        type             integer;
        fname            varchar;
        fast             integer;
        fparams          integer;
        fenv             integer;
        args             integer[] := ARRAY[]::integer[];
        result           integer;
        evda0   constant integer := mal.EVAL(a0, env);
    BEGIN
        SELECT type_id, val_string, ast_id, params_id, env_id
            INTO type, fname, fast, fparams, fenv
            FROM types.value WHERE value_id = evda0;
        FOR i in 1 .. types._count(ast) - 1 LOOP
            args[i] := mal.EVAL(types._nth(ast, i), env);
        END LOOP;
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
CREATE FUNCTION mal.mal_eval(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal.EVAL(args[1], 0);
END; $$ LANGUAGE plpgsql;
INSERT INTO types.value (type_id, val_string) VALUES (11, 'mal.mal_eval');

SELECT envs.vset(0, 'eval',
                   (SELECT value_id FROM types.value
                    WHERE val_string = 'mal.mal_eval')) \g '/dev/null'
-- *ARGV* values are set by RUN
SELECT envs.vset(0, '*ARGV*', mal.READ('()')) \g '/dev/null'


-- core.mal: defined using the language itself
SELECT mal.REP('(def! not (fn* (a) (if a false true)))') \g '/dev/null'
SELECT mal.REP('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))') \g '/dev/null'

CREATE FUNCTION mal.MAIN(pwd varchar, argstring varchar DEFAULT NULL)
    RETURNS integer AS $$
DECLARE
    line      varchar;
    output    varchar;
    allargs   integer;
BEGIN
    PERFORM envs.vset(0, '*PWD*', types._stringv(pwd));

    IF argstring IS NOT NULL THEN
        allargs := mal.READ(argstring);
        PERFORM envs.vset(0, '*ARGV*', types._rest(allargs));
        PERFORM mal.REP('(load-file ' ||
                        printer.pr_str(types._first(allargs)) || ')');
        PERFORM io.close(1);
        PERFORM io.wait_flushed(1);
        RETURN 0;
    END IF;

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
