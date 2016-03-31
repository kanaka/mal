-- env table
CREATE SEQUENCE env_id_seq;
CREATE TABLE env (
    env_id    integer NOT NULL DEFAULT nextval('env_id_seq'),
    outer_id  integer,
    data      hstore
);
ALTER TABLE env ADD CONSTRAINT pk_env_id
    PRIMARY KEY (env_id);
-- drop sequence when table dropped
ALTER SEQUENCE env_id_seq OWNED BY env.env_id;
ALTER TABLE env ADD CONSTRAINT fk_env_outer_id
    FOREIGN KEY (outer_id) REFERENCES env(env_id);

-- -----------------------

-- env_new
CREATE FUNCTION env_new(outer_env integer)
    RETURNS integer AS $$
DECLARE
    e  integer;
BEGIN
    INSERT INTO env (outer_id) VALUES (outer_env)
        RETURNING env_id INTO e;
    --RAISE NOTICE 'env_new: e: %, outer_env: %', e, outer_env;
    RETURN e;
END; $$ LANGUAGE plpgsql;

-- env_new_bindings
CREATE FUNCTION env_new_bindings(outer_env integer,
                                 binds integer,
                                 exprs integer[])
    RETURNS integer AS $$
DECLARE
    bseq  integer[];
    env   integer;
    i     integer;
    bind  integer;
    bsym  varchar;
    expr  integer;
BEGIN
    env := env_new(outer_env);
    bseq := _valueToArray(binds);
    FOR i IN 1 .. COALESCE(array_length(bseq, 1), 0) LOOP
        bind := bseq[i];
        bsym := _valueToString(bind);
        expr := exprs[i];
        --RAISE NOTICE 'i: %, bind: %, expr: %', i, bind, expr;
        IF bsym = '&' THEN
            bind := bseq[i+1];
            PERFORM env_set(env, bind,
                            _list(exprs[i:array_length(exprs, 1)]));
            RETURN env;
        END IF;
        PERFORM env_vset(env, bsym, expr);
    END LOOP;
    RETURN env;
END; $$ LANGUAGE plpgsql;


-- env_vset
-- like env_set but takes a varchar key instead of value_id
CREATE FUNCTION env_vset(env integer, name varchar, val integer)
    RETURNS integer AS $$
DECLARE
    e  integer = env;
    d  hstore;
BEGIN
    SELECT data INTO d FROM env WHERE env_id=e;
    IF d IS NULL THEN
        d := hstore(name, CAST(val AS varchar));
    ELSE
        d := d || hstore(name, CAST(val AS varchar));
    END IF;
    UPDATE env SET data = d WHERE env_id=e;
    RETURN val;
END; $$ LANGUAGE plpgsql;


-- env_set
CREATE FUNCTION env_set(env integer, key integer, val integer)
    RETURNS integer AS $$
DECLARE
    symkey  varchar;
BEGIN
    symkey := _valueToString(key);
    RETURN env_vset(env, symkey, val);
END; $$ LANGUAGE plpgsql;

-- env_find
CREATE FUNCTION env_find(env integer, symkey varchar)
    RETURNS integer AS $$
DECLARE
    outer_id  integer;
    d         hstore;
    val       integer;
BEGIN
    SELECT e.data, e.outer_id INTO d, outer_id FROM env e
        WHERE e.env_id = env;
    IF d ? symkey THEN
        RETURN env;
    ELSIF outer_id IS NOT NULL THEN
        RETURN env_find(outer_id, symkey);
    ELSE
        RETURN NULL;
    END IF;
END; $$ LANGUAGE plpgsql;


-- env_vget
CREATE FUNCTION env_vget(env integer, symkey varchar)
    RETURNS integer AS $$
DECLARE
    result  integer;
    e       integer;
BEGIN
    e := env_find(env, symkey);
    --RAISE NOTICE 'env_find env: %, symkey: % -> e: %', env, symkey, e;
    IF e IS NULL THEN
        RAISE EXCEPTION '''%'' not found', symkey;
    ELSE
        SELECT data -> symkey INTO result FROM env WHERE env_id = e;
    END IF;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- env_get
CREATE FUNCTION env_get(env integer, key integer)
    RETURNS integer AS $$
BEGIN
    RETURN env_vget(env, _valueToString(key));
END; $$ LANGUAGE plpgsql;
