-- ---------------------------------------------------------
-- step9_try.sql

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
CREATE FUNCTION mal.is_pair(ast integer) RETURNS boolean AS $$
BEGIN
    RETURN types._sequential_Q(ast) AND types._count(ast) > 0;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.quasiquote(ast integer) RETURNS integer AS $$
DECLARE
    a0   integer;
    a00  integer;
BEGIN
    IF NOT mal.is_pair(ast) THEN
        RETURN types._list(ARRAY[types._symbolv('quote'), ast]);
    ELSE
        a0 := types._nth(ast, 0);
        IF types._symbol_Q(a0) AND a0 = types._symbolv('unquote') THEN
            RETURN types._nth(ast, 1);
        ELSE
            a00 := types._nth(a0, 0);
            IF types._symbol_Q(a00) AND
               a00 = types._symbolv('splice-unquote') THEN
                RETURN types._list(ARRAY[types._symbolv('concat'),
                                         types._nth(a0, 1),
                                         mal.quasiquote(types._rest(ast))]);
            END IF;
        END IF;
        RETURN types._list(ARRAY[types._symbolv('cons'),
                                 mal.quasiquote(types._first(ast)),
                                 mal.quasiquote(types._rest(ast))]);
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.is_macro_call(ast integer, env integer) RETURNS boolean AS $$
DECLARE
    a0      integer;
    f       integer;
    result  boolean = false;
BEGIN
    IF types._list_Q(ast) THEN
        a0 = types._first(ast);
        IF types._symbol_Q(a0) AND
           envs.find(env, types._valueToString(a0)) IS NOT NULL THEN
            f := envs.get(env, a0);
            SELECT macro INTO result FROM types.value WHERE value_id = f;
        END IF;
    END IF;
    RETURN result;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal.macroexpand(ast integer, env integer) RETURNS integer AS $$
DECLARE
    mac  integer;
BEGIN
    WHILE mal.is_macro_call(ast, env)
    LOOP
        mac := envs.get(env, types._first(ast));
        ast := types._apply(mac, types._valueToArray(types._rest(ast)));
    END LOOP;
    RETURN ast;
END; $$ LANGUAGE plpgsql;

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
    a2       integer;
    let_env  integer;
    idx      integer;
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
  LOOP
    -- PERFORM writeline(format('EVAL: %s [%s]', pr_str(ast), ast));
    SELECT type_id INTO type FROM types.value WHERE value_id = ast;
    IF type <> 8 THEN
        RETURN mal.eval_ast(ast, env);
    END IF;

    ast := mal.macroexpand(ast, env);
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
    WHEN a0sym = 'quote' THEN
    BEGIN
        RETURN types._nth(ast, 1);
    END;
    WHEN a0sym = 'quasiquote' THEN
    BEGIN
        ast := mal.quasiquote(types._nth(ast, 1));
        CONTINUE; -- TCO
    END;
    WHEN a0sym = 'defmacro!' THEN
    BEGIN
        fn := mal.EVAL(types._nth(ast, 2), env);
        fn := types._macro(fn);
        RETURN envs.set(env, types._nth(ast, 1), fn);
    END;
    WHEN a0sym = 'macroexpand' THEN
    BEGIN
        RETURN mal.macroexpand(types._nth(ast, 1), env);
    END;
    WHEN a0sym = 'try*' THEN
    BEGIN
        BEGIN
            RETURN mal.EVAL(types._nth(ast, 1), env);
            EXCEPTION WHEN OTHERS THEN
                IF types._count(ast) >= 3 THEN
                    a2 = types._nth(ast, 2);
                    IF types._valueToString(types._nth(a2, 0)) = 'catch*' THEN
                        binds := ARRAY[types._nth(a2, 1)];
                        exprs := ARRAY[types._stringv(SQLERRM)];
                        env := envs.new(env, types._list(binds), exprs);
                        RETURN mal.EVAL(types._nth(a2, 2), env);
                    END IF;
                END IF;
                RAISE;
        END;
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
SELECT mal.REP('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) ")")))))') \g '/dev/null'
SELECT mal.REP('(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list ''if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons ''cond (rest (rest xs)))))))') \g '/dev/null'
SELECT mal.REP('(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))') \g '/dev/null'

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
