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
CREATE FUNCTION is_pair(ast integer) RETURNS boolean AS $$
BEGIN
    RETURN _sequential_Q(ast) AND _count(ast) > 0;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION quasiquote(ast integer) RETURNS integer AS $$
DECLARE
    a0   integer;
    a00  integer;
BEGIN
    IF NOT is_pair(ast) THEN
        RETURN _list(ARRAY[_symbolv('quote'), ast]);
    ELSE
        a0 := _nth(ast, 0);
        IF _symbol_Q(a0) AND a0 = _symbolv('unquote') THEN
            RETURN _nth(ast, 1);
        ELSE
            a00 := _nth(a0, 0);
            IF _symbol_Q(a00) AND a00 = _symbolv('splice-unquote') THEN
                RETURN _list(ARRAY[_symbolv('concat'),
                                   _nth(a0, 1),
                                   quasiquote(_rest(ast))]);
            END IF;
        END IF;
        RETURN _list(ARRAY[_symbolv('cons'),
                           quasiquote(_first(ast)),
                           quasiquote(_rest(ast))]);
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION is_macro_call(ast integer, env integer)
    RETURNS boolean AS $$
DECLARE
    a0      integer;
    f       integer;
    result  boolean = false;
BEGIN
    IF _list_Q(ast) THEN
        a0 = _first(ast);
        IF _symbol_Q(a0) AND env_find(env, _valueToString(a0)) IS NOT NULL THEN
            f := env_get(env, a0);
            SELECT macro INTO result FROM value WHERE value_id = f;
        END IF;
    END IF;
    RETURN result;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION macroexpand(ast integer, env integer)
    RETURNS integer AS $$
DECLARE
    mac  integer;
BEGIN
    WHILE is_macro_call(ast, env)
    LOOP
        mac := env_get(env, _first(ast));
        ast := _apply(mac, _valueToArray(_rest(ast)));
    END LOOP;
    RETURN ast;
END; $$ LANGUAGE plpgsql;

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
    -- RAISE NOTICE 'EVAL: % [%]', pr_str(ast), ast;
    SELECT type_id INTO type FROM value WHERE value_id = ast;
    IF type <> 8 THEN
        RETURN eval_ast(ast, env);
    END IF;

    ast := macroexpand(ast, env);
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
        binds := (SELECT val_seq FROM value WHERE value_id = a1);
        idx := 1;
        WHILE idx < array_length(binds, 1) LOOP
            PERFORM env_set(let_env, binds[idx], EVAL(binds[idx+1], let_env));
            idx := idx + 2;
        END LOOP;
        env := let_env;
        ast := _nth(ast, 2);
        CONTINUE; -- TCO
    END;
    WHEN a0sym = 'quote' THEN
    BEGIN
        RETURN _nth(ast, 1);
    END;
    WHEN a0sym = 'quasiquote' THEN
    BEGIN
        ast := quasiquote(_nth(ast, 1));
        CONTINUE; -- TCO
    END;
    WHEN a0sym = 'defmacro!' THEN
    BEGIN
        fn := EVAL(_nth(ast, 2), env);
        fn := _macro(fn);
        RETURN env_set(env, _nth(ast, 1), fn);
    END;
    WHEN a0sym = 'macroexpand' THEN
    BEGIN
        RETURN macroexpand(_nth(ast, 1), env);
    END;
    WHEN a0sym = 'try*' THEN
    BEGIN
        BEGIN
            RETURN EVAL(_nth(ast, 1), env);
            EXCEPTION WHEN OTHERS THEN
                IF _count(ast) >= 3 THEN
                    a2 = _nth(ast, 2);
                    IF _valueToString(_nth(a2, 0)) = 'catch*' THEN
                        binds := ARRAY[_nth(a2, 1)];
                        exprs := ARRAY[_stringv(SQLERRM)];
                        env := env_new_bindings(env, _list(binds), exprs);
                        RETURN EVAL(_nth(a2, 2), env);
                    END IF;
                END IF;
                RAISE;
        END;
    END;
    WHEN a0sym = 'do' THEN
    BEGIN
        PERFORM eval_ast(_slice(ast, 1, _count(ast)-1), env);
        ast := _nth(ast, _count(ast)-1);
        CONTINUE; -- TCO
    END;
    WHEN a0sym = 'if' THEN
    BEGIN
        cond := EVAL(_nth(ast, 1), env);
        SELECT type_id INTO type FROM value WHERE value_id = cond;
        IF type = 0 OR type = 1 THEN -- nil or false
            IF _count(ast) > 3 THEN
                ast := _nth(ast, 3);
                CONTINUE; -- TCO
            ELSE
                RETURN 0; -- nil
            END IF;
        ELSE
            ast := _nth(ast, 2);
            CONTINUE; -- TCO
        END IF;
    END;
    WHEN a0sym = 'fn*' THEN
    BEGIN
        RETURN _malfunc(_nth(ast, 2), _nth(ast, 1), env);
    END;
    ELSE
    BEGIN
        el := eval_ast(ast, env);
        SELECT type_id, val_string, ast_id, params_id, env_id
            INTO type, fname, fast, fparams, fenv
            FROM value WHERE value_id = _first(el);
        args := _restArray(el);
        IF type = 11 THEN
            EXECUTE format('SELECT %s($1);', fname)
                INTO result USING args;
            RETURN result;
        ELSIF type = 12 THEN
            env := env_new_bindings(fenv, fparams, args);
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
CREATE FUNCTION mal_eval(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN EVAL(args[1], 0);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, val_string) VALUES (11, 'mal_eval');

SELECT env_vset(0, 'eval',
                   (SELECT value_id FROM value
                    WHERE val_string = 'mal_eval')) \g '/dev/null'
-- *ARGV* values are set by RUN
SELECT env_vset(0, '*ARGV*', READ('()'));


-- core.mal: defined using the language itself
SELECT REP('(def! *host-language* "plpqsql")') \g '/dev/null'
SELECT REP('(def! not (fn* (a) (if a false true)))') \g '/dev/null'
SELECT REP('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) ")")))))') \g '/dev/null'
SELECT REP('(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list ''if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons ''cond (rest (rest xs)))))))') \g '/dev/null'
SELECT REP('(def! *gensym-counter* (atom 0))') \g '/dev/null'
SELECT REP('(def! gensym (fn* [] (symbol (str "G__" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))') \g '/dev/null'
SELECT REP('(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))') \g '/dev/null'


CREATE FUNCTION MAIN_LOOP(pwd varchar)
RETURNS integer AS $$
DECLARE
    line    varchar;
    output  varchar;
BEGIN
    PERFORM env_vset(0, '*PWD*', _stringv(pwd));
    PERFORM REP('(println (str "Mal [" *host-language* "]"))');
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

CREATE FUNCTION RUN(pwd varchar, argstring varchar)
RETURNS void AS $$
DECLARE
    allargs  integer;
BEGIN
    allargs := READ(argstring);
    PERFORM env_vset(0, '*PWD*', _stringv(pwd));
    PERFORM env_vset(0, '*ARGV*', _rest(allargs));
    PERFORM REP('(load-file ' || pr_str(_first(allargs)) || ')');
    PERFORM wait_flushed(1);
    RETURN;
END; $$ LANGUAGE plpgsql;

